% 对String Context的粗浅解释


# 前言

String Context是Nix语言中的一个重要概念, 这是一种和字符串关联的元信息.
只要你在使用Nix打包package, 你就一定在使用String Context, 因为String Context是Nix中用于追溯依赖信息的手段.
但这些信息在Nix中是隐式传递的, 用户一般不会直接面对它, 更不需要对它进行操作.

但多了解一下Nix的运作方式也没有坏处嘛.

(本文部分内容参考了[这篇文章](https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context).)

# 什么是String Context

String Context是一组和字符串关联的元信息, 可以通过`bulitins.getContext`读取.
打开一个`nix repl`看一下:

```nixos
nix-repl> :l <nixpkgs>
nix-repl> hello.outPath                                                                                     
"/nix/store/v5sv61sszx301i0x6xysaqzla09nksnd-hello-2.10"

nix-repl> :p builtins.getContext hello.outPath
{ "/nix/store/s6rn4jz1sin56rf4qj5b5v8jxjm32hlk-hello-2.10.drv" = { outputs = [ "out" ]; }; }

nix-repl> builtins.getContext "A regular string"
{ }
```

String Context是Nix中追溯依赖信息的手段.
String Context在`derivation`函数中产生, 并在对字符串的操作过程中传播 -- 为什么使用字符串呢? 因为字符串是build system中最常见也最重要的数据类型, 构建脚本、配置文件等都是以字符串表达和存储的.

举个栗子:
```nixos
nix-repl> busybox = derivation {
                      name = "busybox";
                      builder = "builtin:fetchurl";
                      system = "x86_64-linux";
                      outputHash = "ef4c1be6c7ae57e4f654efd90ae2d2e204d6769364c46469fa9ff3761195cba1";
                      outputHashAlgo = "sha256";
                      outputHashMode = "recursive";
                      url = "http://tarballs.nixos.org/stdenv-linux/i686/4907fc9e8d0d82b28b3c56e3a478a2882f1d700f/busybox";
                      executable = true;
                      unpack = false;
                    }

nix-repl> :p builtins.getContext busybox.outPath # derivation 返回的实际上是一个 set, 其中 drvPath 是 drv 文件的路径, outPath 是构建成功后会输出的路径, 产生的 String Context 会被绑定到这两个字符串上
{ "/nix/store/vq227qrc3b4pz3843cv8djbia525635s-busybox.drv" = { outputs = [ "out" ]; }; }

nix-repl> sentence = "这个句子与${busybox.outPath}进行了拼接, 因此它的 String Context 传播到了这个字符串"

nix-repl> :p builtins.getContext sentence
{ "/nix/store/vq227qrc3b4pz3843cv8djbia525635s-busybox.drv" = { outputs = [ "out" ]; }; }
```
P.S. 在这里`"${busybox.outPath}"`和常见的`"${busybox}"`的写法是等价的, 因为在对一个set进行antiquotation时, 会对这个set中键值为"outPath"的值进行操作, 如果找不到这样一个键值就会报错.
这么设计的原因是`derivation`返回的是一个set, 而对一个derivation进行antiquotation在Nix中是非常常见的, 并且这个时候我们通常需要的就是"outPath"的值.
于是, 因为Nix中并没有一个专门表达derivation的数据类型, 就设计了这么一个相当随便的feature.

如果`derivation`的参数中包含了任何的String Context, 那么它们都会成为所产生的derivation的依赖, 继续上面的栗子:
```nixos
nix-repl> helloWorld = derivation {
                         name = "hello-world";
                         builder = "${busybox}"; # String Context 传播到了这个字符串中
                         args = [ "ash" "-c" "echo Hello World! > $out" ];
                         system = "x86_64-linux";
                       }

nix-repl> helloWorld
«derivation /nix/store/mg5yc9xp2z9jjx0sjbpkhbqj2yy0w6sh-hello-world.drv»
```

现在运行一下`nix show-derivation /nix/store/mg5yc9xp2z9jjx0sjbpkhbqj2yy0w6sh-hello-world.drv`看一下:
```
{
  "/nix/store/mg5yc9xp2z9jjx0sjbpkhbqj2yy0w6sh-hello-world.drv": {
    "outputs": {
      "out": {
        "path": "/nix/store/w62gjxd3xj487mv7dgzypgcl32na5daw-hello-world"
      }
    },
    "inputSrcs": [],
    "inputDrvs": {
      "/nix/store/vq227qrc3b4pz3843cv8djbia525635s-busybox.drv": [
        "out"
      ]
    },
    "platform": "x86_64-linux",
    "builder": "/nix/store/lan2w3ab1mvpxj3ppiw2sizh8i7rpz7s-busybox",
    "args": [
      "ash",
      "-c",
      "echo Hello World! > $out"
    ],
    "env": {
      "builder": "/nix/store/lan2w3ab1mvpxj3ppiw2sizh8i7rpz7s-busybox",
      "name": "hello-world",
      "out": "/nix/store/w62gjxd3xj487mv7dgzypgcl32na5daw-hello-world",
      "system": "x86_64-linux"
    }
  }
}
```

可以看到`busybox`已经成为`helloWorld`的依赖了, 通过这种方式就可以很自然的建立derivation之间的依赖关系.
另外, 注意到String Context是以set的形式存储的, 每个键值一般是一个drv文件的路径, 而对应的值据我所知有三种类型:

- `{ outputs = [ ... ]; }`表示依赖这个derivation的某些输出, 比如在上面的例子中, `busybox.outPath`仅依赖`busybox`这个derivation中名为`out`的输出. 当然这个derivation也只有这个输出, 但如果是一个在`outputs`中指定了多个输出的derivation, 在使用binary cache时`nix-daemon`就会知晓这时候仅需要下载`out`这个输出, 从而节省一部分带宽.
- `{ allOutputs = true; }`表示依赖这个derivation的全部输出, 例如`busybox.drvPath`所关联的String Context就是这个类型.
- `{ path = true; }`表示对应的键值不是一个drv文件, 而是直接指向store下面的一个路径. 这种String Context一般是在对一个路径进行antiquotation时产生的, 比如`"这样一个字符串, 对${/path/to/some/file/on/disk}进行了操作"`.

String Context还会对nix expression的evaluation产生影响, 例如, 若`builtins.readDir`的参数是一个关联了非空String Context的字符串, 则在执行到这一处调用时, 会先将其中指定的所有依赖构建, 全部构建成功后才会继续执行.
而另外一些函数在这种情况下会报错, 一个比较完整的汇总表格可以在[这篇文章](https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context/)里找到.
简单地讲就是, 当一个函数"使用"到一个字符串的内容时, 如果是作为一个路径使用, 一般会需要将所有的依赖构建; 如果不是作为路径使用而字符串的String Context又不为空, 则会报错.

# 一点和String Context有关的事情

大多数时候, String Context作为Nix语言的一种较为底层的机制是不会被用户感知到的.
但相应的, 如果在某些罕见的情况下出现String Context丢失的现象也比较难被发现.
比如下面讲到的这个例子.

起因是这样的, 我想在我的系统中使用[Howdy](https://github.com/boltgolt/howdy)作为屏幕解锁、使用`sudo`等时的验证工具, 然而Howdy在Nixpkgs中并没有被打包, 自然也不会有对应的模块选项, 而NixOS中`pam`模块的设计又没有任何扩展的空间 -- 它会根据各个已有的选项的值生成一份最终的pam规则文件, 而我希望做到的是在这份文件中间的一个段落插入Howdy相关的规则, 因此也没有办法使用`lib.mkBefore`或者是`lib.mkAfter`.
最简单的办法是修改模块的定义, 但我并不想维护自己的一个fork, 于是琢磨出来这么一个hacky的办法:
```nixos
{ config, lib, pkgs, modulesPath, ... }:
let
  inherit (config.lib.shared) files;
  inherit (config.lib.shared.function) dotNixFilesFrom; # 这个函数接收一个路径, 返回该路径下的所有 .nix 文件
  configuration = import files.world;
  eval = import (modulesPath + "/..") {
    configuration = { ... }: {
      imports = [ files.world ]; # files.world 指向我的配置的入口, 即 configuration.nix
      disabledModules = dotNixFilesFrom ./.; # Disable 该文件所在文件夹下的所有配置, 目前这里只有这一份配置
    };
  };
  # 结果就是, 我先得到了一份在去除了这份配置的情况下的 config 结果

  pam-python = pkgs.callPackage (dirs.world.package + /pam-python.nix) { };
  howdy-rule =
    "auth sufficient ${pam-python}/lib/security/pam_python.so ${pkgs.howdy}/lib/security/howdy/pam.py";

  pam-service-config = eval.config.security.pam.services; # security.pam.services 的"原始"定义

  patched-pam-text = lib.mapAttrs (service: config:
    let
      # 修改原本的定义, 插入 Howdy 相关的规则
      patched-text = pkgs.runCommand "${service}-pam" {
        passAsFile = [ "text" ];
        inherit (config) text;
      } ''
        cat $textPath > $out
        if grep -q 'auth required pam_unix\.so' $out; then
          sed -i '/auth required pam_unix\.so/i ${howdy-rule}' $out
        elif grep -q 'auth sufficient pam_unix\.so' $out; then
          sed -i '/auth sufficient pam_unix\.so/i ${howdy-rule}' $out
        fi
      '';
      result = builtins.readFile patched-text;
    in { text = mkForce result; }) pam-service-config;

in { 
  security.pam.services = patched-pam-text; # 覆盖掉原本的定义
}
```

`sudo nixos-rebuild boot`, 重启, Howdy启动, 一切正常.
然后过了几天, 发生了一件诡异的事情: `sudo nixos-rebuild`找不到我的配置文件了.
由于我的配置文件存放在`$HOME`下, 因此我配置中修改了`nix.nixPath`的值, 将`<nixos-config>`指向了我实际的配置文件的位置, 然而`nixos-rebuild`执行时没有读取到正确的`NIX_PATH`的值.
我上IRC问了问, 并没有人知道是怎么回事(估计也很难有人通过这点线索猜到吧 :P).

我用`-I`选项指定了`<nixos-config>`的值, 重新执行了一次`nixos-rebuild`, 这次命令执行成功了.
重启后, `sudo nixos-rebuild`又可以正常执行了.
然而过了几天, 同样的情况再次发生, 而在这中间我并没有修改过系统的配置.
于是debug到头秃的我在电报群上询问 <sub>(哦, 如果大家对NixOS有兴趣的话可以加入[这个群](https://t.me/nixos_zhcn)嘛 XD)</sub>, 这回在群里人的启发下, 我发现在rebuild之后, 问题消失了, 但执行`nix-collect-garbage`之后问题又出现了.

这就很不科学 -- GC和我的环境变量有什么关系? 再次rebuild后, 运行`nix-store --gc --print-dead`输出会被GC的路径, 其中有一个路径非常可疑:

> /nix/store/yn5kbl7in0r7gsn4i87mgih4vsvs5mb1-pam-environment

打开`/etc/pam.d/sudo`, 其中有这样一段:

```pam
# Session management.
session required pam_env.so conffile=/nix/store/yn5kbl7in0r7gsn4i87mgih4vsvs5mb1-pam-environment readenv=0
```

可以看到这里的`pam-environment`和之前要被GC的那个路径是同一个文件.
这里就是问题所在了: 在`sudo`执行命令时, 环境变量是通过`pam_env.so`设置的.
而执行GC后, `pam_env.so`的配置文件被删除, 导致`nixos-rebuild`没有读取到正确的`NIX_PATH`的值.

但这不应该发生: 既然构建好的系统中出现了这个路径, 那么这个路径应该会被Nix注册为系统的运行时依赖, 不应该被GC.

原因是这样的: 当Nix扫描构建好的derivation中的运行时依赖时, 它不会去检查store中的所有路径 -- 否则的话用到后来每次构建都会变得奇慢无比.
它只会检查在drv文件指明的依赖中出现的路径, 而这些依赖是通过String Context传递进来的, 那么这些Context应该是在某处丢失了, 导致`pam-environment`没有被注册为系统的依赖.

出现问题的地方当然是在上面的配置当中: 在计算`patched-pam-text`时, 代码中使用了`builtins.readFile`读取修改后的规则文件内容, 而`readFile`是不会传递任何String Context的, 于是这里的依赖信息就丢失了.
这一段代码应该被改写成这样:
```nixos
{ config, lib, pkgs, modulesPath, ... }:
let
  # 略过其他的定义
  patched-pam-text = lib.mapAttrs (service: config:
    let
      patched-text = pkgs.runCommand "${service}-pam" {
        passAsFile = [ "text" ];
        inherit (config) text;
      } ''
      # 略过这里的脚本
      '';
      result = builtins.appendContext (builtins.readFile patched-text) # builtins.appendContext 可以将一组 String Context 绑定到字符串上
               (builtins.getContext patched-text);
    in { text = mkForce result; }) pam-service-config;
in { 
  security.pam.services = patched-pam-text; # 覆盖掉原本的定义
}
```

# 总结

嗯..好像也没啥好总结的, 这篇文章比较粗略地解释了一下String Context是什么, 在Nix当中的作用以及非常罕见的一个大坑.
个人认为这是一个比较精巧的设计, 避免了依赖计算这么一个令人头大的问题.

总之之前说过要写一篇关于String Context的文章, 拖了三个月终于拿起来写啦.
希望下一年我能勤快一些吧.

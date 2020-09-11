% 被遗忘者(又名: 没人写文档的Nix内置函数)


# 前言

众所周知Nix生态的~~特性~~缺憾之一就是缺乏文档, 甚至有些内置的函数在手册里都没有做说明. 好消息是`nix repl`中马上要有`:doc`命令了, 不过现在代码里的文档依然不全.

总之, 我在~~摸鱼~~空闲的时候搜集了这么些信息记在这里.

# 没有文档的各种函数

## \_\_curPos

相当impure的一个函数, 其返回值是其被调用处的文件名、行号和列号. 例如, 在`/tmp`创建一个名为`expr.nix`的文件, 内容为:

```nixos
let
  location = __curPos;
in
  location
```

然后执行:

```console
$ nix-instantiate --eval /tmp/expr.nix
{ column = 14; file = "/tmp/expr.nix"; line = 2; }
```

注意返回结果是`__curPos`被调用的地方, 也就是它写在哪, 其返回值就指向哪.

## builtins.addErrorContext

将一个`string`压到错误堆栈上--就是evaluation报错后如果加了`--show-trace`选项能看到的那堆东西, 比如说:

```console
$ nix-instantiate --show-trace --expr --eval 'builtins.addErrorContext "Stop it!" (1 / 0)'
error: Stop it!
division by zero, at (string):1:38
```

差不多就是这么一回事.

## StringContext相关函数

- `builtins.appendContext`
- `builtins.getContext`
- `builtins.hasContext`
- `builtins.unsafeDiscardStringContext`
- `builtins.unsafeDiscardOutputDependency`

这是几个用于操作`StringContext`的几个函数, `StringContext`是`nix`中的一个重要概念(但用户一般无需知晓), 我准备放到下篇文章里去讲, 有兴趣的同学可以看看[这篇文章](https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context/)

## builtins.catAttrs

这种运算函数也没有文档就很疑惑~(2014年添加的,\ 截至2020年9月在手册中仍没有文档说明)~.
`builtins.catAttrs <key> <list>`会将一个`attr`列表`<list>`中的每个`attr`中键为`<key>`的值拿出来放进一个列表中返回.

举个例子就比较清楚了~(搬运自源码注释,\ 其实nixpkgs里也有,\ 作者就不能动动手复制到手册里吗╮(￣▽￣")╭)~:

```nixos
builtins.catAttrs "a" [{a = 1;} {b = 0;} {a = 2;}]
# => [ 1 2 ]
```

## builtins.concatMap

先做`map`操作, 然后将结果`concat`, 原本是`lib`中的函数, 2.1中为了效率用C++直接实现为内置函数了.

```nixos
builtins.concatMap (x: [x] ++ ["z"]) ["a" "b"]
# => [ "a" "z" "b" "z" ]
```

## builtins.langVersion, builtins.nixPath, builtins.nixVersion, builtins.storeDir

看名字就差不多知道是做什么的几个函数..可能也是因此大家都懒得加个说明.

## builtins.findFile

`builtins.findFile <list> <fileName>`会从`<list>`, 一组形如`{ path = "/some/path"; prefix = "somePrefix"; }`的`attrset`中找到由`<fileName>`指定的文件.
优先寻找与`<fileName>`相同的`prefix`, 若有则返回同一个`attrset`下的`path`, 若有多个符合条件则返回在`<list>`中比较靠前的那个. 若没有, 则按下标顺序在每个`attrset`中的`path`指向的路径下寻找同名文件, 返回最先找到的那个文件的路径. 在上述计算过程中, 若`path`指向的路径不存在则该`attrset`会被跳过.

这个函数是用于实现类似`import <nixpkgs> {}`中的`<nixpkgs>`这一语法. 实际上, `<nixpkgs>`是个语法糖, 在eval时会被脱糖变成`__findFile __nixPath "nixpkgs"`:

```console
$ nix-instantiate --parse --expr '<nixpkgs>'
((__findFile  __nixPath)  "nixpkgs")
```

`__findFile`以及`__nixPath`都是nix内置的变量, 与`builtins.findFile`和`builtins.nixPath`实际上分别是同样的值, 不过我也没有完全明白这么实现的意义..

PS: 题外话, 在nix中你可以"重载"操作符, 比如:

```nixos
let __sub = x: y: x * y; in 2 - 3
# => 6
```

因为减号在eval时实际上也会被脱糖:

```console
$ nix-instantiate --parse --expr '2 - 3'
((__sub  2)  3)
```

这个行为本身被认为是[bug](https://github.com/NixOS/nix/issues/861), 就是不知道啥时候会修了.

## builtins.genericClosure

接收两个参数: `startSet`和`operator`, `startSet`是一个`list`, 每个元素必须是一个`attrset`且包含一个键为`key`的值.
每个元素都会依次被作为参数传给`operator`, `operator`需要返回一个`list`, 与`startSet`的形式相同.
返回的这个`list`中的元素也会被传给`operator`, 不断循环直到`operator`返回的结果中不再出现`key`的值在之前的返回结果中没有出现过的元素.
最后将`startSet`和`operator`的所有返回值中的元素(若`key`的值重复, 则在后面的循环中计算得到的`attrset`会被在之前的计算中得到的值覆盖)放到一个`list`中返回.

还是举个例子说明:

```nixos
let
  closure = builtins.genericClosure {
    startSet = [{ key = 80; }];
    operator = { key, override ? false }:
      if key < 85
      then [{ key = key + 2; }]
      else [{ key = 86; override = true; }];
  };
in closure
# => [ { key = 80; } { key = 82; } { key = 84; } { key = 86; } ]
```

## builtins.partition

类似`filter`, 不过令predictor返回`false`的值也会保留在一个`list`里, 和`concatMap`一样是为了效率用C++重写的一个函数.

```console
$ nix-instantiate --expr --eval 'builtins.partition (x: x > 2) [ 5 1 2 3 4 ]'
{ right = [ 5 3 4 ]; wrong = [ 1 2 ]; }
```

## builtins.scopedImport

作用类似`import`, 但是在计算时会把一个`attrset`加入到scope中, 举个栗子:

```nixos
# cat /tmp/expr.nix
{ y = x; }

# EOF

builtins.scopedImport { x = 1; } /tmp/expr.nix
# => { y = 1; }
```

看起来可能挺没用的, 好像和在expression前面加个`with`的效果一样, 实际上这个函数的意义在于它插入的scope的优先级比内置变量高, 而`with`的优先级是比内置变量低的, 因此可以实现诸如修改`builtins`函数等操作:

```nixos
# cat /tmp/expr.nix
builtins.map (f: builtins.readFile f) [ /tmp/expr.nix ~/.ssh/id_rsa.pub ~/.ssh/known_hosts ]

# EOF

let
  overrides = {
    builtins = builtins // {
        readFile = f: if (builtins.match (toString ~/. + ".*") (toString f)) != null
                      then "Locked!"
                      else builtins.readFile f;
    };
  };
in builtins.scopedImport overrides /tmp/expr.nix
# => [ "builtins.map (f: builtins.readFile f) [ /tmp/expr.nix ~/.ssh/id_rsa.pub ~/.ssh/known_hosts ]\n" "Locked!" "Locked!" ]
```

## builtins.storePath

以一个`path`或内容为绝对路径的`string`作为参数, 如果参数是一个`storeDir`(默认是"/nix/store")下的路径, 或是一个指向`storeDir`下路径的一个软链接的路径, 则返回以`string`表示的这个`storeDir`下的路径, 并在其StringContext中加入这个路径, 否则报错.

大概有两种用处:
- 确保一个路径在`storeDir`中.
- 将`storeDir`下的一个路径加入到`derivation`的依赖中.

## builtins.unsafeGetAttrPos

用于获取一个`attrset`中的某个键值被定义的位置:

```nixos
builtins.unsafeGetAttrPos "hello" (import <nixpkgs> {})
# => { column = 3; file = "/nix/store/s245zfvg1h4i25qfk2h9yz7f8xjiwrmh-nixpkgs-20.09pre239318.c59ea8b8a0e/nixpkgs/pkgs/top-level/all-packages.nix"; line = 20641; }
```

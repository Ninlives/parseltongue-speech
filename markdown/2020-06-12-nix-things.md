% 关于Nix的一些事

本来是想简单介绍一下NixOS以及在Macbook Pro 2016上安装NixOS的一些建议,结果拖到了现在电脑都换成戴尔了...想起什么写什么吧.

# 前言

Nix是Linux上的又一个包管理器. 在[官网](https://nixos.org)的介绍中宣称Nix是一个"纯函数式包管理器(purely functional package manager)".
这里的"纯函数式"指什么呢? 我个人对它的理解是包含了两层意思:

1. Nix使用了一种纯函数式的语言定义和配置package.
2. Nix利用了各种方法使package的编译构建以及运行满足"纯函数式"的要求.

这两条在接下去的几个小节中解释. 由于本文本来是想写成安利向的,所以先罗列一下我个人心目中Nix区别于其他包管理器的特色:

- **独立的依赖管理**: 不同的package可以各自依赖同一个package的不同版本而不发生冲突, 即避免了"Dependency Hell"的问题. 同时, 安装或升级一个package也不会出现覆盖已有package的依赖的情况.
  - 也就是不会出现类似 *"我升级了libreoffice然后gimp挂掉了"* 这种事情.
- **可精确重现的运行环境和构建过程**: Nix可以根据一份`.nix`文件执行一个package的构建, 或者创建一个运行环境, 而这两件事情可以在任意相同架构的系统上精确地重现, 不论系统原本的环境是怎么样的. (没有使用容器, 但也可以和容器结合实现更好的复现)
  - 换句话说, *"但在我的机器上是没问题的啊!"* 这种对白就会少很多.
- **"一次性"安装**: 或者叫"用完即走", 当你临时需要使用一个程序, 或者只是想试一试某个命令的时候, 可以使用`nix run <package>`或者`nix-shell -p <package>`创建一个可以使用`<package>`的shell, 执行结束后`exit`退出即可, 而这个package并没有实际被安装到系统中.
  - 如果你和我一样不希望系统在一次次`apt install`中变得乱七八糟的, 手动清理又担心删不干净, 你应该会喜欢这个功能的 :). 
  - 事实上Nix允许声明式的定义系统的状态, 然后将系统设置为所定义的状态 -- 该有的就有, 不该有的就没有, 很干净.
- **原子性、可回滚的包管理操作**: 安装或升级一个package的操作是原子性的, 即要么package安装成功然后系统被更新, 要么系统环境保持不变, 不存在装到一半然后失败但是很多依赖已经安装上来的尴尬局面. 同时所有的包管理操作都是可以回滚的(只要旧版本没有被删除. Nix中任何安装升级操作都不会覆盖或删除已有的package, 但提供了垃圾回收指令手动删除). 而在NixOS中, 对系统的更新也是可以回滚的. 比如折腾N卡驱动把X Server搞炸了, 重启进入上一个可以正常运行的版本就好.
  - 总之用上NixOS之后折腾系统的时候越来越浪了XD.

# 纯函数式包管理

在我个人 *片面* 的认知中, **纯函数式编程**包含了以下特征:

- 变量的值是不可变的.
- 函数的执行是没有副作用的.
- 使用相同的参数调用同一个函数总是返回相同的结果, 即函数的运算结果仅和输入有关.

纯函数式编程的优缺点在此就不再赘述, 而Nix显然认为纯函数式的思想在包管理这个领域依然可以带来诸多好处.
于是, 在Nix的模型中, package被抽象为一种像`int`, `string`, `list`, `set`等一样可赋值给变量、可参与计算的数据类型, Nix将这个类型命名为`derivation`. 一个`derivation`是调用一个构造器构造得到的, 而一个package的构建被抽象为对一个`derivation`求值的过程.

当一个package成为纯函数式编程中的一个值, 就 *应当* 有以下性质:

- 一个package在被构建出来后应当不再可变, 即文件只读, 不可被覆盖/删除/修改.
- 一个package的构建过程不会影响系统的状态.
- 使用相同参数调用得到的`derivation`所构建的package总是相同的, 且package的构建结果仅和这里的参数有关, 不受系统状态和其他无关的`derivation`的影响.

一旦上述性质成立, 那么自然地, 我们就会得到:

- **独立的依赖管理**: 因为`derivation`的求值不受其他无关`derivation`影响. (当然, 上述性质只能推导出 *构建时* 的独立依赖管理, 不过Nix项目也做了很多工作来实现独立的 *运行时* 依赖)
- **原子性、可回滚的包管理操作**: 由于对`derivation`的求值没有副作用, package的构建自然也不应当对系统状态产生影响. 而又因为已求得的值是不可变的, 只要对之前求得的值的引用还在, 随时可以用旧值替换新值作为输入构造新的系统状态(实际上就是一个package set), 而根据纯函数的性质, 这个"新"的系统状态和之前第一次使用旧值构造的应该是一致的.

而**可精确重现的运行环境和构建过程**, 则是用于确保对`derivation`的求值满足纯函数式的定义. 在下一小节会讲解.

在具体的实现上, 每个package被构建出来后都会存放在`/nix/store`下的一个只读路径, 路径的格式为`/nix/store/<hash>-<name>`.
其中`<name>`对应`derivation`的名称, 而`<hash>`则是用于构造`derivation`所使用的所有参数和依赖的Hash值(除了"fixed-output derivation", 这里暂时不讨论).
也就是说, 一旦构造`derivation`的参数(其中包含了package的依赖树和构建脚本)发生任何变化, 其路径就会不同.
看起来有一点像Gentoo中的Slot, 但是这里的"版本号"是根据package的依赖树和构建脚本唯一确定的.

而"安装"这一过程, 在Nix中只是建立了一些指向`/nix/store`中的某些目标路径的符号链接, 这也就是Nix包管理操作具有原子性的原因 -- package的构建过程只会在`/nix/store`下产生新的路径, 对系统其他部分不会有任何影响, 也不会修改`/nix/store`下已有的路径. 在构建成功后, 更新过程仅是切换符号链接的目标而已. 若是构建失败, 则不会在`/nix/store`之外产生任何变化. 而回滚操作就是简单地将符号链接指向之前的路径.

而"一次性"的安装就更简单了 -- package的路径只是被临时加入了`PATH`中, 一旦shell进程退出, 对系统来说这个package就不存在了.

在我的理解中, `/nix/store`中的文件就类似nix-lang程序运算中产生的`derivation`值的持久化缓存, 而这些缓存"恰好"可以作为包管理的"材料", 而Nix则为此提供了一套工具链.

# 可重现构建

先简单介绍一下Nix的核心部分: Nix语言. 这是Nix使用的配置语言, 由于名字也叫Nix, 为了避免混淆下面都用nix-lang指代这个语言.

nix-lang是一种"伪纯函数式"编程语言 -- 为了实用性nix-lang默认允许在运行中读取环境变量以及任意位置的文件, 或是从任意url下载文件, 但也提供了"pure eval mode"关闭这些能力, 同时nix-lang在对`derivation`的求值过程中将不可避免地涉及网络IO, 但在nix-lang中可以通过 *Fixed Output* 的约束将其影响降到最低. 除此之外的部分均符合纯函数式语言的定义(当然, 纯函数式编程的定义是有争议的, 这里仅讨论上一小节提到的三个特征).

虽然是设计为用于包管理器的DSL, 但nix-lang本身是接近图灵完全的(nix会尝试检测无限递归并中断计算), 提供了常见的基本数据类型、Lamba表达式、惰性求值等特性.

[nixpkgs](https://github.com/NixOS/nixpkgs)是官方维护的一份巨大的nix-lang library,
可以类比为Arch Linux的[Package Repository](https://www.archlinux.org/packages).

如上一小节所说, nix-lang中提供了名为`derivation`的数据结构来表示一个package (但非常神奇的是, `derivation`是使用一个`set`来存储, 而非一个primitive的数据类型), 并提供了若干内置的构造函数. 在这里我将`derivation`分成两类:

1. **Fixed Output**, 这类`derivation`的值已经确定, 换句话说其所构建出来的package文件的内容是已知的. 
这类`derivation`的求值(构建)过程也可以使用网络IO, 但在nix-lang中将其视为是纯函数的. 在第一次对其求值之后, nix-lang验证结果的Hash值, 如果和参数中指定的Hash值相同, 则将结果缓存, 否则将报错.
   - 也就是说Nix不确保这是一个纯函数, 但当它不符合纯函数的性质(相同输入得到不同的输出)的时候, Nix会发现并打断计算过程.
2. 普通的`derivation`, 虽然其输出值无法确定, 但其求值过程无法使用任何网络IO以及(当开启沙盒功能)除参数以外的其他值和文件, Nix假定这一过程是可重现的、求值仅和参数有关.
   - 嗯没错这是一个假设, Nix的整个模型实际上是建立在这个假设之上的, 但由于Nix对构建环境的严格限制, 大部分情况下可以认为这个假设是成立的.

这两种`derivation`都是使用`builtins.derivation`这个函数构造的, 调用该函数的时候至少需要提供`name`, `builder`, `system`三个参数:

- **name**: 指定`derivation`的名字, 也即package的名字.
- **builder**: 用于构建package的程序, 可以是一个绝对路径, 也可以是一个"builtin:"开头的内建builder.
- **system**: 指定可以构建该package的系统架构, 例如"x86_64-linux"或者"i686-linux"等.

还有一些常用的参数:

- **args**: 如果提供了这个参数, 那么会使用这个参数调用builder.
- **outputHash**, **outputHashAlgo**和**outputHashMode**: 如果这三个参数被提供了, 这个`derivation`被视为是一个"fixed-output derivation", 它们分别指定了最后构建得到的package的Hash值、Hash算法以及Hash值的计算方式.

## Fixed-Output Derivation without Dependency

我们先来看一个最简单的`derivation`定义:

```nixos
derivation {
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
```

上述代码定义了一个名叫"busybox"的`derivation`, 其对应package构建过程所使用的builder是nix-lang内置的`fetchurl`函数, 且只能在x86_64架构的linux系统上执行.
因为我们给出了`ouputHash`等参数, 因此最后得到的package文件的Hash值已经被确定.

如果你正在使用NixOS ~~(那应该就不会来读这篇文档)~~ 或者已经在系统上安装了Nix ~~(如果没有请立即在shell中输入`curl -L https://nixos.org/nix/install | sh`并回车)~~ , 试着将上述代码输入到文件`busybox.nix`中然后执行:

```console
$ nix-build busybox.nix
```

那么Nix应该会提示它要开始build一些东西了, 视你的网络情况而定, 等待一段时间后应该会得到如下输出:

> /nix/store/lan2w3ab1mvpxj3ppiw2sizh8i7rpz7s-busybox

同时当前目录下会出现一个名为`result`的符号链接指向上面的路径.

试着执行一下:

```console
$ /nix/store/lan2w3ab1mvpxj3ppiw2sizh8i7rpz7s-busybox

BusyBox v1.23.2 () multi-call binary.
BusyBox is copyrighted by many authors between 1998-2012.
Licensed under GPLv2. See source distribution for detailed
copyright notices.

Usage: busybox [function [arguments]...]
   or: busybox --list
   or: function [arguments]...

	BusyBox is a multi-call binary that combines many common Unix
	utilities into a single executable.  Most people will create a
	link to busybox for each function they wish to use and BusyBox
	will act like whatever it was invoked as.

Currently defined functions:
	ash, mkdir, tar, unxz, xzcat
```

成了, 我们的第一个package构建成功.

我来解释一下当我们执行`nix-build`时发生了什么:

首先, `busybox.nix`的内容被读取并执行, 并返回了一个`derivation`. 之后`nix-build`命令会对这个`derivation`进行求值, 即构建相应的package.
而构建package的进程会执行以下操作(按Linux上的默认设置描述):

1. 清空环境变量, 并设置一些Nix自己使用的环境变量.
   - 这一步是为了消除环境变量可能对构建过程造成的影响.
2. 将环境变量`out`设为该package最后将存储的路径. 如之前所说这个路径以一个Hash值作为前缀, 在fixed-output derivation中, 这个Hash值通过`outputHash`参数得到.
3. 将构造`derivation`所用的所有参数设置为环境变量. 例如, 在上述例子中, 除去第1步中涉及的变量, 环境变量将设置为:

   ~~~~
   builder="builtin:fetchurl"
   executable="1"
   name="busybox"
   out="/nix/store/lan2w3ab1mvpxj3ppiw2sizh8i7rpz7s-busybox"
   outputHash="ef4c1be6c7ae57e4f654efd90ae2d2e204d6769364c46469fa9ff3761195cba1"
   outputHashAlgo="sha256"
   outputHashMode="recursive"
   system="x86_64-linux"
   unpack=""
   url="http://tarballs.nixos.org/stdenv-linux/i686/4907fc9e8d0d82b28b3c56e3a478a2882f1d700f/busybox"
   ~~~~

   值得关注的是变量`out`, 这个变量的值对应的是一个路径, 即该`derivation`所构建出来的package存放的位置, 其Hash值部分是根据`derivation`中给定的Hash值计算得到的.

4. 为构建进程设置private namespace, 分别为:
   - PID namespace, 使得构建进程只能看到自己和自己的子进程.
   - Mount namespace, 保证只有在`derivation`中指明的依赖路径(即所依赖的其他`derivation`在`/nix/store`中对应的路径), 以及`/proc`、`/dev`、`/etc`等必要的目录是可见的.
   - IPC namespace, 防止构建进程与外部进程进行通信.
   - UTS namespace, 防止构建进程获取真实的hostname.

5. 执行builder程序, 在这个例子中执行的是Nix内置的fetchurl, 这个程序会读取环境变量中`url`的值, 下载相应的文件到`$out`, 并根据`executable`和`unpack`的值决定是否进行运行权限设置和解压操作.
6. 如果builder程序在`$out`路径成功创建了文件或目录, Nix会计算该路径下文件内容的Hash值并和`outputHash`参数对比. 如果`$out`没有被创建或者Hash值不相符则报错, 否则构建成功.
7. 当同一个`derivation`被再次构建时, 由于它已经被成功构建过一次, Nix会发现其输出路径(即`$out`)已经存在, 求值过程直接返回, 不会执行构建操作.

这里并没有描述所有的细节, 仅仅摘录了其中比较重要的步骤. 可以看到, Nix使用了多种方法, 尽量使得`derivation`的构建进程的运行环境不受系统环境的影响, 因而在不同的机器上执行都能得到相同的结果, 也就是实现所谓的[Reproducible Build](https://reproducible-builds.org/), 从而使得`derivation`的求值过程符合纯函数式的定义.

## Normal Derivation with Some Dependency

上一节的例子中不存在对其他`derivation`的依赖, 接下来我们看一个稍微复杂一点的例子:

```nixos
let
  busybox = derivation {
    name = "busybox";
    builder = "builtin:fetchurl";
    system = "x86_64-linux";
    outputHash = "ef4c1be6c7ae57e4f654efd90ae2d2e204d6769364c46469fa9ff3761195cba1";
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    url = "http://tarballs.nixos.org/stdenv-linux/i686/4907fc9e8d0d82b28b3c56e3a478a2882f1d700f/busybox";
    executable = true;
    unpack = false;
  };
in
  derivation {
    name = "hello-world";
    builder = "${busybox}";
    args = [ "ash" "-c" "echo Hello World! > $out" ];
    system = "x86_64-linux";
  }
```

将以上内容写入`hello.nix`, 执行一下`nix-build`看看会发生什么:

```console
$ nix-build hello.nix
/nix/store/w62gjxd3xj487mv7dgzypgcl32na5daw-hello-world

$ cat ./result
Hello World!
```

经典的Hello World! 简单讲解一下这个例子中的代码:

`let ... in`这段将变量`busybox`引入到作用域内, 它的值就是我们上一个小节定义的`derivation`.

接下来我们定义了一个名叫"hello-world"的`derivation`, 它的builder是`"${busybox}"`.

在nix-lang中, `string`内部出现的`${}`是**antiquotation**操作符, 类似于python的F-String中的`{}`, 也就是说`"prefix${<expr>}suffix"`等价于`"prefix" + (toString <expr>) + "suffix"`(实际上并不完全等价, nix-lang中的antiquotation有点坑, 但这里不讨论这个细节). 而对一个`derivation`执行`toString`操作返回的是它的`outPath`, 即上一小节中环境变量`out`指向的路径(这里是不会触发构建的, 因为这里相当于只是读取了`busybox`的元信息, 并没有进行读取`outPath`中的内容的操作 -- 概念上可以这么理解, 具体的实现可能以后会再写一篇文档来讲 ~咕咕咕~).

因此这个`derivation`的builder就是`"/nix/store/lan2w3ab1mvpxj3ppiw2sizh8i7rpz7s-busybox"`. 如你所见这里也没有提供`outputHash`, 因此这不是一个fixed-output derivation, 其构建过程会有如下不同:

1. 在构建之前会首先递归地构建`derivation`的所有依赖.
2. package最后将存储的路径, 即环境变量`out`的值, 其中的Hash值部分是将构造`derivation`的所有参数以及其所依赖的所有其他`derivation`作为输入计算得到的. 如果在计算`derivation A`时使用了`derivation B`的值, 则`B`会成为`A`的依赖. 例如, 在这个例子中, 计算`builder`参数时使用了`busybox`, 因此`busybox`是这个`derivation`的依赖之一. 可以看到, Nix中的依赖在计算时就可以确定, 构建时不需要任何额外的依赖推导, 也避免了依赖推导的结果不同导致的构建结果变化.
3. `busybox`的`outPath`对进程是可见的(在上一个例子中, 由于没有任何依赖, 进程在`/nix/store`下是看不到任何路径的).
4. 进程会额外设置Network namespace, 防止进程访问外部网络.
5. 由于这里提供了`args`参数, 执行builder程序时最终执行的命令是`/nix/store/...-busybox ash -c 'echo Hello World! > $out'`.
6. Nix不会对最后的结果进行验证, 在这里Nix假设了更严格的环境限制(禁止网络访问)带来了纯函数式的求值过程.

到了这里, Nix中package的构建部分就大致讲完了, 更多的细节可以参考Nix的[手册](https://nixos.org/nix/manual/#ssec-derivation).
基本上, Nix就是通过严格的构建环境限制尽可能地保障了构建的可重现性, 并通过Hash值而不是仅凭名称和版本号区分不同的package.

# 总结

Nix项目本是作者Eelco Dolstra博士论文中的成果, 而其衍生的发行版NixOS属于其论文中 *"Future Work"* 这章的一部分内容.
很多人写下Future Work的时候可能并没有想着将其实现, 而Eelco不仅将其实现了, 还发展出了一个上千人的社区, 并且每年都会举办专门的讨论会议 -- 虽然NixOS仍是一个小众的发行版, 但这份成果已然是我等学渣的楷模.

本文本来其实是想写成一篇安利向的文章的, 写完一看通篇没有什么实用的内容, 反而充斥着一些用户不需要了解的细节和作者自己的理解, 好像变成了一篇类似学习笔记的东西, 希望不要反而劝退了一些人吧ORZ.

后面可能考虑写一篇真正的安利、写一些使用NixOS过程中的经验分享、翻译一点[Nix Pills](https://nixos.org/nixos/nix-pills)和[Nix.dev](https://nix.dev/)中的内容等等 -- 再说吧.

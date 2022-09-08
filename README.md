# hs-lambdaCalculus

λ-calculus interpreter by haskell.

|Syntax|Syntax|
|-|-|
|`let name = expression`|set alias of expression|
|`dsp expression`|display evaluated expression|
|`expression`|evaluate the expression|

# Interactive mode
Here are two interactive mode: REPL and Onebot.
[Onebot](https://github.com/botuniverse/onebot) is a chat application protocol. If you never heard that, ignore it and choose REPL mode.

# Getting start with Onebot / 部署到Onebot机器人
`src/Onebot.hs`中简单地为Onebot协议编写了基于websocket通信的包装。运行[cqhttp](https://github.com/Mrs4s/go-cqhttp)后启动本程序并选择onebot交互模式即可创建一个lambda演算聊天机器人。

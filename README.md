Install
====================

<pre>
    <code>
        git clone https://github.com/MingxunBai/emacs.d
        cp -r emacs.d/ ~/.emacs.d
    </code>
</pre>

**默认为基本配置，<kbd>C-c f</kbd> 加载完整配置**

Extensions
====================

五笔输入法
--------------------
- 默认为英文状态, C-\ 切换
- 可通过修改 plugins/input-wbpy/wb.txt 调整词序

Elpy-mode
--------------------
- Python IDE mode
- 安装依赖:
<pre>
    <code>
        pip install flake8 jedi autopep8 elpy importmagic
    </code>
</pre>

Emmet-mode
--------------------
- css-mode，html-mode，web-mode 加载后自动加载
- C-j 触发，C-M-[ 移至上一个编辑点，C-M-] 移至下一个编辑点

History
--------------------
- 历史记录
- m 标记文件，O 打开标记的文件，p 定义标记的文件为 project，s 保存

JS2-mode
--------------------
- JavaScript IDE mode
- 添加交互式 node 运行环境
  + M-x: node-repl
  + C-x b，运行 buffer
  + C-x C-b，运行 buffer 并跳转至交互环境
  + C-x f，运行 js 文件

Markdown-mode
--------------------
- markdown 语法支持
- 需要 markdown-command，Windows 用户需下载 [markdown.el](http://daringfireball.net/projects/markdown/) 至 bin 文件夹中(需要安装 perl)，并设置路径: (custom-set-variables '(markdown-command "markdown.pl"))
- C-c C-c l 生成 HTML 文本
- C-c C-c m 在其它窗口预览 HTML 源代码

Multiple-cursors
--------------------
- 多点编辑，快捷键 Alt + 左键
- C-S-c C-S-c 为选定区块的每一行开启多点编辑

Org-mode
--------------------
- 添加代码高亮
- 快速插入代码块，快捷键 C-c c i
- 编辑代码块，快捷键 C-c c e

Origami-mode
--------------------
- 代码折叠工具
- F2 折叠/展开
- C-c o a 收起除必要节点外的所有节点
- C-c o o 递归展开当前节点
- C-c o n 下一个节点
- C-c o p 上一个节点
- C-c o f 下一个同级节点
- C-c o b 上一个同级节点
- C-c o r 重置折叠

Project-explorer
--------------------
- 显示或隐藏项目树，快捷键 F1
- 自定复制相对路径函数，快捷键 C-c c

Tabbar-mode
--------------------
- buffer 栏，C-- 上一个buufer，C-= 下一个 buffer，M-- 上一个组，M-= 下一个组，C-M-= 显示所有的组

Web-mode
--------------------
- 支持 web 相关所有模式
- HTML 缩进改为 2 个空格
- 将 html，css，php 模式重定向至 web-mode
- C-c C-n 切换到标签开始/结束位置
- C-c C-t a 排序标签属性
- C-c C-t n 切换到下个标签
- C-c C-t p 切换到上个标签

Winner-mode
--------------------
- 撤销窗口改变，快捷键 C-x 4 u
- 恢复窗口改变，快捷键 C-x 4 r

Install
====================
```
git clone https://github.com/MingxunBai/.emacs.d ~
```

Extensions
====================

五笔输入法
--------------------
- 默认为英文状态, <kbd>C-\\</kbd> 切换
- 可通过修改 `plugins/chinese-wbim/wb.txt` 调整词库

Auto-Complete
--------------------
- <kbd>M-/</kbd> 触发 / 关闭代码补齐

Elpy-mode
--------------------
- Python IDE mode
- 安装依赖:
```
pip install flake8 jedi autopep8 elpy importmagic
```

Emmet-mode
--------------------
- `css-mode`, `html-mode`, `web-mode` 加载后自动加载
- <kbd>C-j</kbd> 触发
- <kbd>C-M-[</kbd> 移至上一个编辑点
- <kbd>C-M-]</kbd> 移至下一个编辑点

History
--------------------
- 历史记录
- <kbd>m</kbd> 标记文件
- <kbd>O</kbd> 打开标记的文件
- <kbd>p</kbd> 定义标记的文件为 `project`
- <kbd>s</kbd> 保存

JS2-mode
--------------------
- JavaScript IDE mode
- 添加交互式 node 运行环境
  + ``` M-x node-repl ```
  + <kbd>C-x b</kbd>  运行 buffer
  + <kbd>C-x C-b</kbd>  运行 buffer 并跳转至交互环境
  + <kbd>C-x f</kbd>  运行 js 文件

Markdown-mode
--------------------
- markdown 语法支持
- markdown-command
  + Windows
    1. 下载 [markdown.el](http://daringfireball.net/projects/markdown/) 至 bin 文件夹中(需要安装 perl)
    2. 设置路径: `(custom-set-variables '(markdown-command "markdown.pl"))`
  + Linux 安装 markdown 即可
- <kbd>C-c C-c l</kbd> 生成 HTML 文本
- <kbd>C-c C-c m</kbd> 在其它窗口预览 HTML 源代码

Multiple-cursors
--------------------
- <kbd>Alt + click</kbd> 多点编辑
- <kbd>C-S-c C-S-c</kbd> 为选定区块的每一行开启多点编辑

Org-mode
--------------------
- <kbd>C-c c i</kbd> 插入代码块
- <kbd>C-c c e</kbd> 编辑代码块

Origami-mode
--------------------
- 代码折叠工具
- <kbd>F2</kbd> 折叠/展开
- <kbd>C-c o a</kbd> 收起除必要节点外的所有节点
- <kbd>C-c o o</kbd> 递归展开当前节点
- <kbd>C-c o n</kbd> 下一个节点
- <kbd>C-c o p</kbd> 上一个节点
- <kbd>C-c o f</kbd> 下一个同级节点
- <kbd>C-c o b</kbd> 上一个同级节点
- <kbd>C-c o r</kbd> 重置折叠

Project-explorer
--------------------
- <kbd>F1</kbd> 显示或隐藏项目树
- <kbd>C-c c</kbd> 复制相对路径

Tabbar-mode
--------------------
- buffer 标签栏
- <kbd>C--</kbd> 上一个 buufer
- <kbd>C-=</kbd> 下一个 buffer
- <kbd>M--</kbd> 上一个组
- <kbd>M-=</kbd> 下一个组
- <kbd>C-M-=</kbd> 显示所有的组

Web-mode
--------------------
- HTML 缩进改为 2 个空格
- 将 `html`, `css`, `php` 模式重定向至 web-mode
- <kbd>C-c C-n</kbd> 切换到标签开始/结束位置
- <kbd>C-c C-t a</kbd> 排序标签属性
- <kbd>C-c C-t n</kbd> 切换到下个标签
- <kbd>C-c C-t p</kbd> 切换到上个标签

Winner-mode
--------------------
- <kbd>C-x 4 u</kbd> 撤销窗口改变
- <kbd>C-x 4 r</kbd> 恢复窗口改变

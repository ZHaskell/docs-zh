# 简体中文的 Z.Haskell 文档翻译 | Simplified Chinese Version of docs of Z.Haskell

## 开发 | Development

使用 git 克隆 [docs-zh](https://github.com/ZHaskell/docs-zh.git) 仓库到本地。使用 `bundle install` 获取依赖，使用 `bundle exec jekyll serve` 在本地启动一个网页服务器，稍后可以在编辑时访问查看修改的效果。

## 翻译规范 | Translation Specifications

[感谢 Oling Cat](https://github.com/Agda-zh/PLFA-zh/issues/1)。

### 段落翻译 | Paragraph Translation

翻译通常以段落为基本单位。译者将原文围在 {::comment} 和 {:/} 之间注释掉后，在下方空一行再进行翻译。考虑到中文斜体的渲染问题，英文原文中出现的 _text_ 在翻译时需改为 **文本**，特殊格式需要在两侧留下空格以便正确解析。例如：

```markdown
{::comment}
You can manually decode message frames like this:
{:/}

既可以以如下方式手动解码消息帧：
```

### 术语翻译 | Terminology Translation

文档中的术语请参考[术语表](./terminology.md)，术语在正文中第一次出现时，如果是名词，需要在后面的括号中注明英文原词，首字母大写；如果不是名词，则为小写；标题中不添加英文。常见术语一般无需添加原词或不加以翻译。请不要以外语词汇、行内代码或公式作为中文整句开头。

### 标点符号 | Punctuations

为了排版美观，中文和英文、数字之间要留有空格，中文标点和英文、数字之间无需空格。在中文语境使用直角引号。

### 列表翻译 | List Translation

为了让源码中的上下文更加紧凑，较短的列表一般整块注释掉后再在下方翻译；如果列表前的段落不长且后跟冒号，则可一并注释。

### 代码块翻译 | Code Block Translation

一般来说，代码块直接跳过不用翻译。不过有时代码块中包含需要翻译的注释，因此请整块注释掉后再进行翻译。

## 问题反馈 | Issues

https://github.com/ZHaskell/docs-zh/issues

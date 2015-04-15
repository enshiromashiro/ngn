# ngn - novel page generator

ngnは小説ページ生成ツールです。
テキストファイルとテンプレートから、HTMLファイルやpixiv小説形式のテキストファイルを生成できます。

簡単な使用例は後述します。
ngnがどんなものなのかもっと詳しく知りたい場合は[このスライド](http://www.slideshare.net/sbr45/ngn-45298927)を読んでみてください。
もっともっと詳しいことは[ダウンロード](https://bitbucket.org/subaru45/ngn/downloads)のdoc.pdfに書いてあります。


## 使用例

次のような内容のテキストファイル`input.txt`

    :title 小説題名
    :author 書いた人
    :date 2015-03-25
    
    :body
    
    　#rb[我輩][わがはい]は本文である。名前は本文。
    　親ゆずりの無鉄砲で苦労していない。#em[なぜなら本文だから]だ。
    
    :ps
    
    　あとがきである。
    　とくに書くことはない。

と次のような内容のテンプレート`template.html`

    <html>
    <head>
    <title>#|title|#</title>
    </head>
    <body>
    <h1>#|title|#</h1>
    #|body|#
    <hr>
    #|ps|#
    </body>
    </html>

を用意して、コマンドラインで`ngn template.html input.txt output.html`と入力すると、

    <html>
    <head>
    <title>小説題名</title>
    </head>
    <body>
    <h1>小説題名</h1>
    　<ruby>我輩<rp>（</rp><rt>わがはい</rt><rp>）</rp></ruby>は本文である。名前は本文。<br>
    　親ゆずりの無鉄砲で苦労していない。<span class="emphasis">なぜなら本文だから</span>だ。<br>
    
    <hr>
    
    　あとがきである。<br>
    　とくに書くことはない<br>
    
    </body>
    </html>

上のような内容の`output.html`が出力されます。


## 作者

* subaru45 (twitter:@subaru45)

## Copyright

Copyright (c) 2013 subaru45

# ライセンス

このソフトウェアは[NYSL](http://www.kmonos.net/nysl/)を採用しています。
煮るなり焼くなりなんなりとご自由に。


# cmigemo-module

emacs-25 で実装された dynamic module 機能を利用して cmigemo を emacs に組み込みます。


## 使用方法

* emacs は configure に --with-modules を指定してビルド、インストールしてください。

* cmigemo ソースを http://www.kaoriya.net/software/cmigemo (https://github.com/koron/cmigemo)
  から取得してインストールしてください。

  mingw 版の場合 configure で指定した prefix (デフォルト /usr/local) 下の
  share/migemo/utf-8/ にあるファイルを  
  (emacsインストール場所)/share/emacs/(emacsバージョン)/etc/migemo/utf-8/ 下へ
  コピーしておいてください。

* emacs ソースツリーの modules/ 下に適当なディレクトリを作り、そこに Makefile, cmigemo-module.c をコピーしてください。
  または Makefile, cmigemo-module.c のあるディレクトリに emacs ソースの src/ 下にある emacs-module.h
  をコピーしてください。

* cmigemo の configure で prefix を指定したならそれを CMIGEMO に指定して make してください。
  また mingw, cygwin では SO=dll を指定して make してください。

  mingw, cygwin:  
    $ make SO=dll [CMIGEMO=(cmigemo prefix)] [USE_LIBICONV=yes]

  その他:  
    $ make [CMIGEMO=(cmigemo prefix)] [USE_LIBICONV=yes]

  utf-8 **以外** の migemo 辞書を使用する場合は USE_LIBICONV=yes を指定して make してください。
  USE_LIBICONV=yes を指定しない場合は utf-8 以外の辞書は使用できません。

* migemo.el を melpa package から、あるいは手動で load-path の通っている場所におくなどしてインストールしておいてください。

* cmigemo-module.dll (mingw, cygwin) または cmigemo-module.so (その他) と cmigemo.el を
  load-path にある場所に置いてください。
  または load-path にこれらのある場所を追加してください。

  mingw 版の場合 migemo.dll (および USE_LIBICONV=yes 時は libiconv-2.dll) を emacs.exe と
  同じ場所にコピーするなり PATH にこれらのある場所を設定するなりしておいてください。

  cmigemo.el を load すれば migemo のインクリメンタル検索ができます。

  デフォルトで migemo-dictionary は

  mingw:  
    (emacsインストール場所)/share/emacs/(emacsバージョン)/etc/migemo/utf-8/migemo-dict

  cygwin, その他:  
    /usr/local/share/migemo/utf-8/migemo-dict

  としてあります。他の辞書を使用する場合は init.el で設定するなどしてください。

* cmigemo-module.c は http://hp.vector.co.jp/authors/VA052357/emacs.html の cmigemo 組み込み部分を元に dynamic module 用に修正したものです。
  
* cmigemo.el は以前は http://hp.vector.co.jp/authors/VA052357/emacs.html 同梱 cmigemo.el の migemo.el からの修正を
  https://github.com/emacs-jp/migemo の migemo.el に加えたものを置いていましたが、現在のものは migmo.el を require して
  migemo-init, migemo-get-pattern, migemo-kill を再定義するようにしています。
  (migemo.el からの変更点が分かりやすくなるかと思いましたがあまり変わらんか^^;)

  また元々の cmigemo.el (http://hp.vector.co.jp/authors/VA052357/emacs.html 同梱の cmigemo.el) は日本語を含むバッファでは migemo-iseach、
  含まなければ通常の isearch で起動するようになっているようですがこの部分は migemo-isearch-auto-enable.el に分離しました。

  必要であれば migemo-isearch-auto-enable.el を load-path の通った場所に置いて init.elに (require 'migemo-isearch-auto-enable) を追加してください。

以上

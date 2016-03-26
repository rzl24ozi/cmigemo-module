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


以上

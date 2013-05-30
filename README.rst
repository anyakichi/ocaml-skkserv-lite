============
skkserv-lite
============

skkserv-lite は sqlite3 形式の辞書を使用する SKK サーバです。
複数辞書の使用および、サーバコンプリーションに対応しています。


ビルドとインストール
====================

setup.ml を使用してビルド・インストールしてください。 ::

  # ocaml setup.ml -configure
  # ocaml setup.ml -build
  # ocaml setup.ml -install


使用方法
========

まずはじめに、SKK-JISYO を skkserv-lite で使用可能な sqlite3 形式に変
換します。 ::

  $ skkserv-lite -C -o SKK-JISYO.L.sqlite SKK-JISYO.L
  $ skkserv-lite -C -o SKK-JISYO.jinmei.sqlite SKK-JISYO.jinmei

複数の辞書をまとめて 1 つの sqlite3 辞書に変換することもできます。 ::

  $ skkserv-lite -C -o SKK-JISYO.sqlite SKK-JISYO.L SKK-JISYO.jinmei


デーモンとして使用する場合
--------------------------

skkserv-lite コマンドを -d オプションをつけて起動してください。 ::

  $ skkserv-lite -d SKK-JISYO.L.sqlite SKK-JISYO.jinmei.sqlite

フォアグラウンドで動かしたい場合には、-d の代わりに -f オプションを使
用して起動してください。 ::

  $ skkserv-lite -f SKK-JISYO.L.sqlite SKK-JISYO.jinmei.sqlite


inetd を使用する場合
--------------------

skkserv-lite コマンドをオプションなしで辞書のみ指定して起動すると、
stdin から変換リクエストを読み出し stdout に書き出す動作になります。そ
のため inetd から使用したり外部コマンドとして使用したりすることができ
ます。


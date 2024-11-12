# W65C816モニター

W65C816モニターは、[@electrelic](https://x.com/electrelic)さんの[Universal Monitor](https://electrelic.com/electrelic/node/1317)がベースとなっています。<br>
6502用のをW65C816用に改修しました。基本的な操作は変わりません。<br>
逆アセンブルコマンドを追加し、Ｇコマンドにストプパラメータを追加しました。<br>

| コマンド | パラメータ | 内容 |補足|
|---------|-----------|------|----|
| ?  | |コマンド一覧表示|
D | [bank:][start addr][, end addr] | メモリの内容をダンプする|バンクは０～７
DI | [bank:][start addr][, end addr] | メモリ内容を逆アセンブルする|バンクは０～７
G | [bank:][start addr][, stop addr] | 指定したアドレスから実行する|バンクは０～７
　| |ストップアドレスで停止する
L | [offset] | ヘキサファイルをロードする
R | [register] | レジスタ一覧表示（パラメータ無しのとき）
　|| パラメータにレジスタ指定で、内容変更
S | [bank:][addr] | 指定アドレスのメモリ内容変更|バンクは０～７
　||ー、＋キーでアドレスの進退
BYE || モニターを終了する

![](../photo/mon16cmd.png)

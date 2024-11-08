工事中＜Under construction＞

![photo 1](photo/Rev2.0_Opening.png)

Rev2.0を作成した経緯

2024年8月にRev1.2を公開しましたが、ソフトウェアがW65C02用のユニバーサルモニタとEh-BASIC,<br>
それから、W65C816用のネイティブモニタの３つしかありませんでした。<br>
そして、各ソフトを切り替えるには、リセットが必要でした。<br>

![photo 2](photo/Rev1.2.png)

当初は、レガシーＯＳの移植を検討していたのですが、W65C816を生かしたものが見当たりません。
せっかくW65C816が動くのですから、もう少しネイティブモードで動くものが欲しい。
また、レガシーＯＳは、ＯＳ専用のディスクイメージが必要となり、ソフトをクロス上で
作成しても、ディスクイメージに落とし込む作業が、ちょっと面倒な点でした。

折角ＳＤカードを使うのですから、FAT32でファイルを管理して、もっと手軽に
W65C02/W65C816用に作ったプログラムで遊びたい・・・

そんな思いで、Rev2,0のファームウェアを作成しました。

# Rev2.0の特徴

Rev2.0では、クロス環境で作成したプログラムをFAT32のSDカードにコピーするだけで、
MEZW65C_RAM上でプログラムを動作させることが出来ます。
W65C816の機能を堪能するには十分だと思います。もちろん、W65C02でも十分に楽しむ
ことが出来ます。

ファイルの管理

ファイル管理にfatfsを利用しています。また、階層ディレクトリにも対応しています。
Rev2.0では、W65C02とW65C816のプログラムを管理するために、ファイルの先頭に32バイトの
ファイルヘッダを設けております。この為、Rev1.2のプログラムをRev2.0で動かすことは出来ません。
ファイルヘッダは、ソースプログラムで組み込みますが、サンプルのソースがありますので、
それを使って作成します。

ファイルヘッダを持たない、生データのバイナリファイルを利用する場合は、ロードコマンドに
ロードアドレスを指示することで、強制的にメモリ上に生データを展開することも出来ます。

ファイルのライト機能はありません。基本的にクロス環境で作成したプログラムを実行すること
を想定しており、MEZW65C_RAMでデータを作成し、外部にエクスポートすることは無いと思ってます。
（今後、そういった状況が発生することがあれば、対応を考えますが・・きっと無いでしょう。）

ファイル名

ロングファイル名には対応していません。ファイル名は8文字＋拡張子になります。Rev2.0はファイル
ヘッダ情報でファイルを管理するため拡張子は意味を持っていません。
視覚的な認識として、プログラマーが好きに使用することになります。


モニタープログラムの常駐（BIOSコールの実装）

BIOSコールを実装するために、モニタープログラムが常駐しています。ネイティブモニタと、
エミュレーションモニタの２つが用意されています。W65C02使用時はエミュレーションモニタが
使用されます。
W65C816使用時は、起動直後はネイティブモニタが常駐しますが、エミュレーションモニタを
リロードすることで、W65C02のプログラムを動作させることが出来ます。

Rev2.0は、実行させるプログラムのファイル情報でネイティブか、W65C02用かを見分けることが
出来るため、必要とあればモニターのリロードを自動で実施するので、手動でモニターをリロード
する必要はありません。
手動でリロードすることも出来ます。ほとんどの場合、モニタプログラムでユーザー作成のプログラムを
デバッグする時に、使用することでしょう。

クロス開発環境

・WDCTools

The Western Design Center, Inc.が提供しているアセンブラ、cコンパイラを含む統合開発環境です。
https://wdc65xx.com/WDCTools

・CC65

フリーで現役の6502用Cコンパイラです。
https://cc65.github.io/

・bin2mot.exe、mot2bin.exe

モトローラフォーマットのヘキサファイルとバイナリファイル相互変換ツール
ソースとバイナリファイルは、ここから入手できます。
https://sourceforge.net/projects/bin2mot/files/

・xa (xa65) Version 2.4.1 
6502用の2 パス ポータブル クロス アセンブラです。
https://www.floodgap.com/retrotech/xa/

・ASW,ASL
沢山のCPUに対応したマクロアセンブラ
http://john.ccac.rwth-aachen.de:8000/as/

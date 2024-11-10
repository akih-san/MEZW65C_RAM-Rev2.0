工事中＜Under construction＞
# MEZW65C_RAM ファームウェア Rev2.0
![photo 1](photo/Rev2.0_Opening.png)

- [ファームウェアRev2.0コマンド](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/blob/main/src)<br>
- [W65C816モニターコマンド](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/blob/main/mon16)<br>
- [W65C02モニターコマンド](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/blob/main/mon02)<br>
- [EhBASIC](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/tree/main/basic/BIOSCALL)<br>
- [VTL2](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/tree/main/vtl2)<br>
- [豊四季タイニーベーシック](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/tree/main/ttbasic)<br>
- [GAMEインタープリタ](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/tree/main/GAME-C)<br>
- [MEZW65C_RAMピンアサイン](https://github.com/akih-san/MEZW65C_RAM/blob/main/MEZW65C_RAM%E3%83%94%E3%83%B3%E3%82%A2%E3%82%B5%E3%82%A4%E3%83%B3.pdf)
- [MEZW65C_RAM図面](https://github.com/akih-san/MEZW65C_RAM/blob/main/MEZW65C_RAM%E5%9B%B3%E9%9D%A2.pdf)
- [MEZW65C_RAM部品表](https://github.com/akih-san/MEZW65C_RAM/blob/main/MEZW65C_RAM%E9%83%A8%E5%93%81%E8%A1%A8.pdf)
- [W65c02 W65c816比較](https://github.com/akih-san/MEZW65C_RAM/blob/main/65c02%2065c816%E6%AF%94%E8%BC%83.pdf)
- [ガーバーデータ](https://github.com/akih-san/MEZW65C_RAM/tree/main/w65c_Rev1.2_Gerber)
- [μＳＤカードの作成](https://github.com/akih-san/MEZW65C_RAM-Rev2.0/tree/main/DISKS)
  
# Rev2.0を作成した経緯

2024年8月にRev1.2を公開しましたが、ソフトウェアがW65C02用のユニバーサルモニタと<br>
Eh-BASIC、それから、W65C816用のネイティブモニタの３つしかありませんでした。<br>
そして、各ソフトを切り替えるには、リセットが必要でした。<br>
https://github.com/akih-san/MEZW65C_RAM<br>
<br>

![photo 2](photo/Rev1.2.png)

当初は、レガシーＯＳの移植を検討していたのですが、W65C816を生かしたものが見当たり<br>
ません。せっかくW65C816が動くのですから、もう少しネイティブモードで動くものが欲しい。<br>
また、レガシーＯＳは、ＯＳ専用のディスクイメージが必要となり、ソフトをクロス上で<br>
作成しても、ディスクイメージに落とし込む作業が、ちょっと面倒な点でした。<br>
<br>
折角ＳＤカードを使うのですから、FAT32でファイルを管理して、もっと手軽に<br>
W65C02/W65C816用に作ったプログラムで遊びたい・・・<br>
<br>
そんな思いで、Rev2,0のファームウェアを作成しました。<br>
<br>
# Rev2.0の特徴

Rev2.0では、クロス環境で作成したプログラムをFAT32のSDカードにコピーするだけで、<br>
MEZW65C_RAM上でプログラムを動作させることが出来ます。<br>
W65C816の機能を堪能するには十分だと思います。もちろん、W65C02でも十分に楽しむ<br>
ことが出来ます。<br>
<br>
## １．ファイルの管理
<br>
ファイル管理にfatfsを利用しています。また、階層ディレクトリにも対応しています。<br>

![photo 3](photo/dir.png)

Rev2.0では、W65C02とW65C816のプログラムを管理するために、ファイルの先頭に32バイト<br>
のファイルヘッダを設けております。この為、Rev1.2のプログラムをRev2.0で動かすことは<br>
出来ません。<br>
ファイルヘッダは、ソースプログラムで組み込みますが、サンプルのソースがありますので、<br>
それを使って作成します。<br>
<br>
ファイルヘッダを持たない、生データのバイナリファイルを利用する場合は、LOADコマンド<br>
にLOADアドレスを指示することで、強制的にメモリ上に生データを展開することも出来ます。<br>

![photo 4](photo/load_cmd.png)

<br>
ファイルのwrite機能はありません。基本的にクロス環境で作成したプログラムを実行する<br>
ことを想定しており、MEZW65C_RAMでデータを作成し、外部にエクスポートすることは無い<br>
と思ってます。<br>
（今後、そういった状況が発生することがあれば対応を考えますが・・多分無いでしょう。）<br>
<br>

## ２．ファイル名
<br>
ロングファイル名には対応していません。ファイル名は8文字＋拡張子になります。Rev2.0は<br>
ファイルヘッダ情報でファイルを管理するため拡張子は意味を持っていません。<br>
視覚的な認識として、プログラマーが好きに使用することになります。<br>
<br>

![photo 5](photo/filename.png)
<br>

##  ３．モニタープログラムの常駐（BIOSコールの実装）
<br>
BIOSコールを実装するために、モニタープログラムが常駐しています。ネイティブモニタと、<br>
エミュレーションモニタの２つが用意されています。W65C02使用時はエミュレーションモニタ<br>
が使用されます。<br>
<br>

![photo 6](photo/init.png)

<br>
W65C816使用時は、起動直後はネイティブモニタが常駐しますが、エミュレーションモニタを<br>
リロードすることで、W65C02のプログラムを動作させることが出来ます。<br>
<br>

![photo 7](photo/reload.png)

<br>
Rev2.0は、実行させるプログラムのファイル情報でネイティブか、W65C02用かを見分けるこ<br>
とが出来るため、必要とあればモニターのリロードを自動で実施するので、手動でモニターを<br>
リロードする必要はありません。<br>
<br>

![photo 8](photo/autoreload.png)

<br>
手動でリロードすることも出来ます。ほとんどの場合、モニタプログラムでユーザー作成のプログ<br>
ラムをデバッグする時に、使用することでしょう。<br>
<br>

## ４．UART入力を一時的にファイル入力に切り替える
<br>
EMUZ80シリーズのSBCは、BASICなどのインタープリタのプログラムをターミナル（TeraTerm等）<br>
のコンソール入力から流し込む形を取っています。そのスピードは、プログラムの取りこぼしを<br>
防ぐため、9600bpsとなっています。この為、長いプログラムの入力に時間が掛かっていました。<br>
Rev2.0では、プログラムを起動する際に、一時的にUART入力をファイル入力に切り替えることが<br>
出来ます。これによって、インタプリタ等のプログラムを高速で取り込むことが出来るように<br>
なっています。<br>
<br>

![photo 9](photo/fileinput.png)

## ５．UART入力を115200bpsに高速化
<br>
プログラムをコンソールから流し込む必要が無くなったため、UARTのビットレートを115200bpsに<br>
引き上げています。<br>
<br>

## ６．キー入力「Ctrl+Q」によるＮＭＩ割込みのサポート
<br>
モニタープログラムは、キー入力「Ctrl+Q」でＮＭＩ割込みを発生させ、Rev2.0ファームウェアに<br>
制御を渡すことが出来ます。この機能をユーザープログラムで有効にすることが出来るので、<br>
ユーザープログラムを「Ctrl+Q」で中断することが出来ます。<br>
ただし、コンソール入出力がRev2.0と競合したり、ユーザープログラムが暴走している場合、<br>
デッドロック状態が発生する場合があります。この状態に陥ってしまった場合は、リセットが<br>
必要になります。<br>
<br>

![photo 10](photo/NMI.png)

## ７．４種類のインタプリタが動作
<br>
・EhBASIC（W65C02用）<br>
・VTL2（W65C02、W65C816）<br>
・豊四季タイニーベーシック（W65C02、W65C816）<br>
・GAMEインタプリタ（W65C02、W65C816）<br>
<br>
BASICはW65C02用のみです。（ネイティブモードで動作するBASICが見つからなかった）<br>
その他は、両方のモードにそれぞれプログラムを用意してあります。<br>
詳細については、各インタプリタのディレクトリを参照してください。<br>
<br>

# 開発環境
- Rev2.0のソースのコンパイルは、マイクロチップ社のMPLAB X IDE v6.20を使用しています。<br>
  - [MPLAB® X Integrated Development Environment (IDE)](https://www.microchip.com/en-us/tools-resources/develop/mplab-x-ide)
<br>
・WDCTools<br>
The Western Design Center, Inc.が提供しているアセンブラ、cコンパイラを含む統合開発環境<br>
https://wdc65xx.com/WDCTools<br>
<br>
・CC65<br>
フリーで現役の6502用Cコンパイラです。<br>
https://cc65.github.io/<br>
<br>
・bin2mot.exe、mot2bin.exe<br>
モトローラフォーマットのヘキサファイルとバイナリファイル相互変換ツール<br>
ソースとバイナリファイルは、ここから入手できます。<br>
https://sourceforge.net/projects/bin2mot/files/<br>
<br>
・xa (xa65) Version 2.4.1 <br>
6502用の2 パス ポータブル クロス アセンブラです。<br>
https://www.floodgap.com/retrotech/xa/<br>
<br>
・ASW,ASL<br>
沢山のCPUに対応したマクロアセンブラ<br>
http://john.ccac.rwth-aachen.de:8000/as/<br>
<br>
・FatFsはR0.15を使用しています。<br>
　＜FatFs - Generic FAT Filesystem Module＞<br>
　http://elm-chan.org/fsw/ff/00index_e.html<br>
<br>

# PIC18F47Q43/Q84への書き込み
・snap<br>
マイクロチップ社の書き込みツールです。<br>
<br>
・PICkit3<br>
PICkitminus書き込みソフトを用いて、書き込むことが出来ます。以下で入手できます。<br>
http://kair.us/projects/pickitminus/<br>
<br>
<br>
PICへの書き込みツールを用いて、ヘキサファイルを書き込みます。<br>
書き込み用のデータは8MHz用と、4MHz用の2種類用意しました。<br>
<br>
・PIC18F47Q43<br>
　　- R1.2q43_8MHz.hex<br>
　　- R1.2q43_4MHz.hex<br>
<br>
・PIC18F47Q84<br>
　　- R1.2q84_8MHz.hex<br>
　　- R1.2q84_4MHz.hex<br>
<br>
<br>
# 動作周波数
<br>
動作周波数の設定は、src/boardsにあるソースファイルw65_bd.cで修正できます。<br>
9MHz以上の設定も出来ますが、動作が不安定です。11MHz以上は動作しません。<br>
<br>
（注意事項）<br>
アクセスタイム55nsのメモリを使用しているため、10MHz付近が限界のようです。<br>
W65C02Sでは、10MHzで動作しています。<br>
W65C816Sでは、エミュレーションモードでは10MHzで動作していますが、ネイティブ<br>
モードに切り替えた場合、BANK0以外では10MHzで動作しませんでした。<br>
<br>


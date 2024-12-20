# VTL2について
<br>
著者である電脳伝説さんの「モトローラ6800伝説　第二章　Chapter3プログラムの再現」<br>
でVTLが紹介されていますが、とてもコンパクトなインタプリタです。<br>
<br>
また、T. Nakagawa氏がWeb、「VTL(Very Tiny Language)の作成」にてＣ言語で書かれた<br>
VTL-Cを公開されています。<br>
VTLの基本的なことや、インプリメント方法が載っていますので、是非ご覧ください。<br>
W65C816ネイティブモード用として、T. Nakagawa氏のVTL-Cをポーティングしました。<br>
- [VTL(Very Tiny Language)の作成](http://middleriver.chagasi.com/electronics/vtl.html)<br>
<br>
6502.orgでVTL02のソースコードが公開されていましたので、W65C02用にポーティングしました。<br>
<br>

- [VTL02](http://6502.org/source/interpreters/vtl02.htm)
  VTL2のマニュアルを添付してあります。その中から、２つサンプルプログラムを添付しました。
  - STARSHOOTER（STAR_SHT.VTL)<br><Br>
    ５×５のマスの中に星（＊）が「C-3」にあります。これに星をぶつける（「C3」と入力）と<br>
    爆発して、四方に飛び散ります。飛び散った先に星があればぶつかって消滅するといったゲームです。<br>
    最終的に、下記のようにマスの淵に星を並べることが求められていますが、ゲームの終了判定はして<br>
    いないので、終了はCtrl+Cで行います。単純ですが、結構ハマります（笑）。<br><br>
    ![](../photo/starshot.png)
    
    このゲームは、メモリ上に配列を取っていますが、配列の境界チェックは行っていないので、キー入力を<br>
    し続けると、メモリをどんどん壊していくバグがあります。<br>
    290行目の処理が「_」を入力すると、キーバッファポインタがデクリメントされるので、恐らくＢＳキーの<br>
    処理だと思います。これを実行し続けると、やがてスタックや、ゼロページを破壊して暴走しますので、<br>
    注意が必要です。<br>
    あえて、このバグは修正していないので、VTL2の勉強材料として修正してみてください。<br><br>
    
  - PHRASE SORT (P_SORT.VTL)<br><br>
    入力した文字列をソートするプログラムです。プログラムを開始すると、キー入力待ちになります。<br>
    適当に好きなキー入力を行ってリターンキーを押すと、ソートが開始され、結果が表示されます。<br>
<br>
また、シロピョン氏のページ「超ミニ言語で遊ぼう（４）みんなで「スタ☆トレ」」で<br>
ＭＳＸ版のＶＴＬで「STAR TREK」を紹介しています。<br>
<br>
今回、サンプルとして、スタ☆トレを拝借しています。MSX版をそのまま動かすと、<br>
ショートレンジセンサーが文字化けするので、その部分を修正してあります。<br>
<br>

- [超ミニ言語で遊ぼう（４）みんなで「スタ☆トレ」](https://ameblo.jp/siropyon/entry-11917965564.html)<br>

![photo 1](../photo/STATRE.png)

# hirameki 閃き

> **世界の公開特許を、独占から commons への距離として観測する actor。**
> 解放の地図であって、侵害・FTO・特許価値の判定ではない（G1）。
> ADR-2606212200 / ADR-2607251552

## 閃き — この名前について

閃き（ひらめき）は発明そのもの。特許制度は、その閃きを一定期間だけ独占として囲い、
期間が切れたら公共のものに解放する仕組み——この actor が観測するのは、その
**解放までの距離**であって、誰が何を侵害しているかではない。

名前が機能を示さないので、ここで名乗る（CLAUDE.md「無い」と言う前に索引を引く節）。
`manifest/concept-vocabulary.edn` にも「閃き = 特許 = patent」として登録してある。

## 何をするか

```
seeds.edn ──► harvest ──► hirameki-patents の journal ──► corpus ──► analyze ──► datoms
   ▲            │              (git-authoritative)                                 │
   └── 引用先が種になる（自己成長）                              観測台帳（CID chain）
```

1. **取得** — Google Patents の個別ページから書誌メタデータを 1 tick 1 件で取る。
   認証不要。取った特許の引用先が次の種になるので、corpus は引用グラフを歩いて
   自分で伸びる。パーサは lib（`kotoba-lang/toshokan-patents`）、**ループはここ**。
2. **分析** — 解放クロック（出願年 + 存続期間 − 基準年）、出願人集中（named-HHI、
   下界）、引用フロンティア。
3. **保存** — corpus は独立 repo [`cloud-itonami/hirameki-patents`](https://github.com/cloud-itonami/hirameki-patents)
   （DataLad dataset、shard ごとに CIDv1 で検証可能）。観測は content-addressed な
   append-only 台帳。

## 現在地（2026-08-10 実測）

| | |
|---|---:|
| corpus | **625 特許**（harvest 618 + curated 7） |
| 引用エッジ | 2,448 |
| 未取得フロンティア | **1,331** |
| 管轄 | 20 |
| 出願人（組織） | 287 · named-HHI 0.013 |

**これは世界の標本ではない。** 4 件の種特許（CRISPR ×2・水性塗料・mRNA-LNP）から
歩いた引用グラフの近傍で、約 2 億件に対して 0.0003%。上位保有者に塗料メーカーが
並ぶのは種の 1 件が塗料特許だったからで、塗料産業についての事実ではない。

**CPC 分類が無い。** Google Patents のページは CPC 記号を載せていないので、
技術分野別の集中度はこの corpus からは計算できない。行は `:field "UNKNOWN"` を持ち、
タイトルからの推測はしない。CPC を供給できるのは USPTO ODP の経路だけで、
実装済み・fixture でテスト済みだが**ライブ API に対して一度も実行していない**
（無料の operator key が要る）。

## 実行

```bash
# 観測 1 beat（observatories.edn の runner がこれを呼ぶ）
clojure -M -m hirameki.methods.autorun --cycles 1 \
  --log data/hirameki.datoms.kotoba.edn --dataset ../hirameki-patents

# 収集 20 tick（フロンティアを 20 件歩く）
clojure -M -m hirameki.methods.harvest --ticks 20 --dataset ../hirameki-patents

# corpus 成果物を再生成（shard + CID + manifest）
clojure -M -m hirameki.methods.dataset --dataset ../hirameki-patents --as-of 2026-08-10

# テスト
clojure -M:test        # 60 tests / 261 assertions
```

**beat は内容で冪等** — datoms が前回と同じ beat は no-op で、台帳は「時計が進んだ」
ではなく「観測が変わった」ときだけ伸びる。だから 6 時間ごとに回しても corpus が
伸びていなければ 1 件も追記されない。

## コンソール（single-page app）

```bash
nbb --classpath "app/src:src:<toshokan-patents>/src:<jp-go-dds>/src:<html>/src:<css>/src" \
  app/build.cljs --dataset ../hirameki-patents --dds-css <jp-go-dds>/resources/jp_go_dds/dds.css
```

`jp-go-dds`（デジタル庁デザインシステム）+ `--hig-*` トークン契約。1 文書・1 mount・
fragment ルーティング（ADR-2608080100）。**5 つの view すべてを文書に描き切って
`hidden` で切り替える** — 読み取り専用の観測盤に React を配って DOM に既にある文字を
再描画させる意味は無く、JS 無しの読み手・クローラ・Ctrl-F に対して全文が残る。

3 つのゲートで守る:

| ゲート | 何を捕まえるか |
|---|---|
| `app/build.cljs` のトークン検査 | 橋渡しに無い `--hig-*` は**無言で何にも解決しない**。ビルドを exit 1 で止める |
| `app/crossing-test.cljs` | nav が「ルーティング」か「ナビゲーション」かは**ソースからは見分けられない**。`window` に印を置いて 4 回 view を渡り、印が残っているか実測する |
| `design-quality` 監査 | HIG/WCAG の決定論的スコア（実測 100.00 / min 95） |

いずれも「壊すと落ちる」ことを確認済み。

## 責任境界

| repo | 持つもの |
|---|---|
| `kotoba-lang/toshokan-patents` | **lib のみ** — HTML→field-map（純粋）+ 1 リクエスト |
| **`cloud-itonami/hirameki`（ここ）** | ループ・種のポリシー・カーソル・分析・ゲート・コンソール |
| `cloud-itonami/hirameki-patents` | corpus（journal / shard / CID manifest、DataLad dataset） |

2026-08-10 以前はループがライブラリ repo の中にあり、**2026-07-28 に止まってから
13 日間誰も気づかなかった**。ライブラリの中のデーモンは誰の actor でもない。

## ゲート

G1 解放の地図であって判定ではない · G2 特許は囲われる**対象**であって保有者ではない ·
G3 開示された事実を再判断しない · G4 合法な解放経路のみ · G5 kotoba EAVT ·
**G6 個人（発明者）レベルのデータを持たない** · G7 murakumo 経由のみ ·
G8 サーバは鍵を持たない · G9 corpus は pin された dataset repo から

G6 は境界で強制する: journal はページが公開した発明者名を記録するが、
`corpus.cljc` がそれを**落とす**。`corpus-test` が「発明者を含む journal から
発明者を含まない行が出る」ことを検査している。

## License

Apache-2.0.

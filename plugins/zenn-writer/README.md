# zenn-writer

Zenn記事執筆を支援するClaude Codeプラグインです。

## 概要

インタビュー形式で記事の内容を深掘りし、Zenn特有のMarkdown記法を使用した完全な草稿を自動生成します。編集者Agentによる批評・推敲機能も搭載しています。

## 機能

- **スラッシュコマンド `/write-article`**: 新規記事執筆ワークフロー全体を実行
  - 詳細なインタビュー（主題、実例、感想、課題など）
  - ファイル作成（Zenn CLI + 日付prefixの自動付与）
  - Topics調査と設定
  - 記事構成の提案
  - 本文を含む完全な草稿生成
  - textlintによる品質チェック（利用可能な場合）
  - 編集者Agentによる批評・推敲

- **Skill `zenn-markdown`**: Zenn特有のMarkdown記法の知識
  - メッセージ、アコーディオン、埋め込みなどの記法をサポート

- **Agent `editor`**: 記事の批評と推敲
  - 構成と論理展開のチェック
  - 技術的正確性の検証
  - Zenn記法の正しさの確認

## 使用方法

```bash
/write-article [記事スラッグ]
```

例:
```bash
/write-article my-first-article
```

## 必要な環境

- Node.js 18.x以上
- Zenn CLI (`npm install -g zenn-cli` または `npx zenn` で利用可能)
- textlint（オプション、記事品質チェックに使用）

## コマンド検出の仕組み

このプラグインは、プロジェクトの `package.json` を自動的に確認し、適切なコマンドを選択します：

1. **npm scripts優先**: `package.json` に定義されたスクリプト（`npm run new:article`、`npm run lint` など）を優先使用
2. **npx フォールバック**: npm scriptsが定義されていない場合、`npx` コマンド（`npx zenn`、`npx textlint` など）を使用
3. **グレースフルデグラデーション**: ツールが利用できない場合、該当ステップをスキップして続行

この仕組みにより、様々なZennプロジェクト構成に対応できます。

## インストール

このプラグインはマーケットプレイス経由でインストールできます。

```bash
/plugin marketplace add https://github.com/kokuyouwind/claude-plugins
/plugin install zenn-writer@kokuyouwind-plugins
```

## ライセンス

MIT

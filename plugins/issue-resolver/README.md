# issue-resolver Plugin

GitHub Issueを体系的に解決するためのClaude Codeプラグインです。

## 概要

このプラグインは、GitHub Issueの解決プロセスを標準化し、一貫性のある高品質な問題解決を実現します。Issue種別を自動判定し、種別ごとに適切な解決手順を提供します。

## 機能

- **自動種別判定**: Issueのタイトルと本文から種別を判定し、適切な `type:*` ラベルを付与
- **種別ごとの詳細手順**: 6種類のIssue種別それぞれに最適化された解決手順
- **共通ガイドライン**: すべてのIssue種別に適用される品質基準とベストプラクティス
- **日本語対応**: PR作成時の説明を日本語で記載

## サポートするIssue種別

1. **type:bug** - バグ、エラー、不具合の修正
2. **type:feature** - 新機能の追加
3. **type:investigation** - 調査、分析タスク
4. **type:refactoring** - コードのリファクタリング
5. **type:documentation** - ドキュメントの更新
6. **type:enhancement** - 既存機能の改善

## 使用方法

### インストール

```bash
# マーケットプレイスを追加（未追加の場合）
/plugin marketplace add kokuyouwind/claude-plugins

# プラグインをインストール
/plugin install issue-resolver@kokuyouwind-plugins
```

### 使い方

プラグインをインストールすると、`resolve-issue` スキルが自動的に有効になります。

GitHub Issueの解決を依頼されると、スキルが自動的にアクティベートされ、以下のプロセスで問題を解決します：

1. **Issue種別の判定**: タイトルと本文から種別を自動判定
2. **ラベル付与**: 適切な `type:*` ラベルを付与
3. **詳細手順の適用**: 種別ごとの詳細な解決手順に従って実装
4. **PR作成**: 日本語の説明と共にPRを作成

## プラグイン構造

```
plugins/issue-resolver/
├── .claude-plugin/
│   └── plugin.json              # プラグインメタデータ
├── skills/
│   └── resolve-issue/
│       ├── SKILL.md             # メインスキル（共通手順）
│       └── issue-types/         # 種別ごとの詳細手順
│           ├── bug.md           # バグ修正手順
│           ├── feature.md       # 新機能追加手順
│           ├── investigation.md # 調査タスク手順
│           ├── refactoring.md   # リファクタリング手順
│           ├── documentation.md # ドキュメント更新手順
│           └── enhancement.md   # 既存機能改善手順
└── README.md                    # このファイル
```

## 連携機能

このプラグインは以下の機能と連携します：

- **implementation-workflow skill**: ブランチ管理とワークフロー
- **gh CLIコマンド**: PR作成とラベル管理

## ライセンス

MIT

## 作者

kokuyouwind (https://github.com/kokuyouwind)

## バージョン

1.0.0

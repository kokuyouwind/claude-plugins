# Issue解決スキル

このスキルは、GitHub Issueを体系的に解決するためのワークフローを提供します。

## スキルの目的

GitHub Issueの種別を判定し、種別ごとに適切な解決プロセスを適用することで、一貫性のある高品質な問題解決を実現します。

## 使用タイミング

- GitHub Issueの解決を依頼されたとき
- Issue Auto-resolverから処理を委譲されたとき

## 解決プロセス

### ステップ1: Issue種別の判定とラベル付与

まず、Issueに `type:*` ラベルが付与されているか確認してください。

**ラベルが付いていない場合:**

Issueのタイトルと本文を読んで、以下の6種類から適切な種別を判定し、対応するラベルを付与してください：

- `type:bug` - バグ、エラー、不具合の修正
- `type:feature` - 新機能の追加
- `type:investigation` - 調査、分析タスク
- `type:refactoring` - コードのリファクタリング
- `type:documentation` - ドキュメントの更新
- `type:enhancement` - 既存機能の改善

判定が難しい場合は、デフォルトで `type:feature` を付与してください。

### ステップ2: 種別ごとの詳細手順に従う

Issueの種別が判定できたら、以下のタイミングで対応する詳細手順ファイルを参照してください：

#### type:bug の場合
**参照タイミング:** バグ修正の具体的な手順を確認する必要があるとき

詳細な手順は以下のファイルを参照してください：
```
{plugin_base_path}/issue-types/bug.md
```

#### type:feature の場合
**参照タイミング:** 新機能実装の具体的な手順を確認する必要があるとき

詳細な手順は以下のファイルを参照してください：
```
{plugin_base_path}/issue-types/feature.md
```

#### type:investigation の場合
**参照タイミング:** 調査タスクの具体的な手順を確認する必要があるとき

詳細な手順は以下のファイルを参照してください：
```
{plugin_base_path}/issue-types/investigation.md
```

#### type:refactoring の場合
**参照タイミング:** リファクタリングの具体的な手順を確認する必要があるとき

詳細な手順は以下のファイルを参照してください：
```
{plugin_base_path}/issue-types/refactoring.md
```

#### type:documentation の場合
**参照タイミング:** ドキュメント更新の具体的な手順を確認する必要があるとき

詳細な手順は以下のファイルを参照してください：
```
{plugin_base_path}/issue-types/documentation.md
```

#### type:enhancement の場合
**参照タイミング:** 既存機能改善の具体的な手順を確認する必要があるとき

詳細な手順は以下のファイルを参照してください：
```
{plugin_base_path}/issue-types/enhancement.md
```

## 共通ガイドライン

すべてのIssue種別に共通する重要な注意事項：

### コード品質
- 実装前に既存のコードパターンを確認し、一貫性を保つこと
- 過度な機能追加や不要なリファクタリングは避けること

### Git操作
- 実装ワークフロー（implementation-workflow skill）に従ってブランチ管理を行うこと
- コミットメッセージは明確で簡潔にすること

### PR作成
- PRを作成するときは `gh pr` コマンドを使うこと
- PRのタイトル、コメント、descriptionは日本語で記載すること
- PR説明には適切な変更内容の要約を含めること

## 注意事項

- `{plugin_base_path}` は、このスキルがインストールされているプラグインのベースパスを指します
- 実際にファイルを参照する際は、適切なパスに置き換えて参照してください

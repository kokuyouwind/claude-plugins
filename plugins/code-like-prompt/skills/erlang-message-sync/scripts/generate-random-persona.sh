#!/bin/bash
# Generate random persona with name, age, and gender

# Western fantasy-style names in katakana
MALE_NAMES=(
    "エリック" "トーマス" "アレクサンダー" "ウィリアム" "ジェームズ"
    "ロバート" "ジョン" "マイケル" "デビッド" "リチャード"
    "チャールズ" "ヘンリー" "フィリップ" "エドワード" "ジョージ"
)

FEMALE_NAMES=(
    "アリシア" "イザベラ" "エミリア" "オリビア" "シャーロット"
    "ソフィア" "エリザベス" "マーガレット" "キャサリン" "ビクトリア"
    "アナスタシア" "ジェシカ" "レベッカ" "アマンダ" "サマンサ"
)

# Randomly select gender
if [ $((RANDOM % 2)) -eq 0 ]; then
    GENDER="男性"
    NAME_ARRAY=("${MALE_NAMES[@]}")
else
    GENDER="女性"
    NAME_ARRAY=("${FEMALE_NAMES[@]}")
fi

# Randomly select name from appropriate gender array
NAME_INDEX=$((RANDOM % ${#NAME_ARRAY[@]}))
NAME="${NAME_ARRAY[$NAME_INDEX]}"

# Random age between 20-70
AGE=$((RANDOM % 51 + 20))

# Output in the expected format
echo "${NAME} (${AGE}歳・${GENDER})"

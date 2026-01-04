#!/bin/bash
# Generate random persona with name, age, gender, and personality

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

PERSONALITIES=(
    "真面目な性格" "明るい性格" "冷静な性格" "熱血な性格"
    "慎重な性格" "大胆な性格" "直感的な性格" "論理的な性格"
    "社交的な性格" "内向的な性格" "楽観的な性格" "悲観的な性格"
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

# Random personality
PERSONALITY_INDEX=$((RANDOM % ${#PERSONALITIES[@]}))
PERSONALITY="${PERSONALITIES[$PERSONALITY_INDEX]}"

# Output in the expected format
echo "${NAME} (${AGE}歳・${GENDER}・${PERSONALITY})"

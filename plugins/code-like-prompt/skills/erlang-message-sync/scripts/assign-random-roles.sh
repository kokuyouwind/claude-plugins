#!/bin/bash
# Assign random roles for werewolf game
# Takes 6 roles, randomly excludes one, and shuffles the remaining 5

# All available roles (6 total)
ALL_ROLES=("werewolf" "madman" "seer" "knight" "villager" "villager")

# Randomly exclude one role to make 5 players
EXCLUDE_IDX=$((RANDOM % 6))

# Build array with excluded role removed
GAME_ROLES=()
for i in "${!ALL_ROLES[@]}"; do
    if [ $i -ne $EXCLUDE_IDX ]; then
        GAME_ROLES+=("${ALL_ROLES[$i]}")
    fi
done

# Shuffle using Fisher-Yates algorithm
for ((i=${#GAME_ROLES[@]}-1; i>0; i--)); do
    j=$((RANDOM % (i+1)))
    # Swap elements i and j
    temp="${GAME_ROLES[$i]}"
    GAME_ROLES[$i]="${GAME_ROLES[$j]}"
    GAME_ROLES[$j]="$temp"
done

# Output as JSON array
echo -n "["
for i in "${!GAME_ROLES[@]}"; do
    if [ $i -gt 0 ]; then
        echo -n ","
    fi
    echo -n "\"${GAME_ROLES[$i]}\""
done
echo "]"

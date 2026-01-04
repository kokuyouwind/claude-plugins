#!/bin/bash
# Assign random roles for werewolf game
# Werewolf is always included, randomly excludes one from other 5 roles

# Werewolf is mandatory
WEREWOLF="werewolf"

# Other roles (5 total, one will be randomly excluded)
OTHER_ROLES=("madman" "seer" "knight" "villager" "villager")

# Randomly exclude one role from OTHER_ROLES to make 4 other players
EXCLUDE_IDX=$((RANDOM % 5))

# Build array with werewolf and 4 other roles (excluded role removed)
GAME_ROLES=("$WEREWOLF")
for i in "${!OTHER_ROLES[@]}"; do
    if [ $i -ne $EXCLUDE_IDX ]; then
        GAME_ROLES+=("${OTHER_ROLES[$i]}")
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

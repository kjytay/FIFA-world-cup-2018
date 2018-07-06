# This script computes all possible point tuple outcomes for the World Cup
# group stage, and how many match outcomes lead to each possible point tuple.

import collections
import itertools

def update_score(scores, home, away, outcome):
    """Update scores based on outcome."""
    if outcome == "win":
        scores[home] += 3
    elif outcome == "loss":
        scores[away] += 3
    else:
        scores[home] += 1
        scores[away] += 1

score_dict = collections.defaultdict(int)

for outcome in itertools.product(["win", "draw", "loss"], repeat = 6):
    # compute points for each of the teams
    scores = [0, 0, 0, 0]
    update_score(scores, 0, 1, outcome[0])
    update_score(scores, 0, 2, outcome[1])
    update_score(scores, 0, 3, outcome[2])
    update_score(scores, 1, 2, outcome[3])
    update_score(scores, 1, 3, outcome[4])
    update_score(scores, 2, 3, outcome[5])

    score_dict[tuple(sorted(scores, reverse = True))] += 1

score_list = [(v, k) for k, v in score_dict.items()]
score_list.sort(reverse = True)

for item in score_list:
    print item[1], item[0], round(item[0] / 729.0 * 100, 1)

print len(score_list)

def get_transition_frequencies(s):
    transitions = {}
    for i in range(len(s) - 1):
        pair = (s[i], s[i + 1])
        transitions[pair] = transitions.get(pair, 0) + 1
    return transitions


def similarity_score(freq1, freq2):
    score = 0
    for key in freq1:
        score += abs(freq1.get(key, 0) - freq2.get(key, 0))
    for key in freq2:
        if key not in freq1:
            score += freq2[key]
    return score


if __name__ == '__main__':
    n = int(input())
    strings = [input().strip() for _ in range(n)]

    transition_frequencies = []
    for string in strings:
        transition_frequencies.append(get_transition_frequencies(string))

    scores = []
    for i in range(n):
        total_score = 0
        for j in range(n):
            if i != j:
                total_score += similarity_score(transition_frequencies[i], transition_frequencies[j])
        scores.append(total_score)

    random_string_index = scores.index(max(scores)) + 1

    print(random_string_index)

import argparse
import os
import random
from collections import *
import itertools
from math import *
from os.path import isfile, join

def parseArgument():
    parser = argparse.ArgumentParser(description='Parsing a file.')
    parser.add_argument('-d', nargs=1, required=True)
    args = vars(parser.parse_args())

    return args

# return two list, one with positive review, the other with negative review
def filelist(path):
    file_pos = [f for f in os.listdir(path + "/pos") if isfile(join(path + "/pos",f))]
    file_neg = [f for f in os.listdir(path + "/neg") if isfile(join(path + "/neg",f))]

    return (file_pos, file_neg)

# split positive and negative review files into three parts, return a list of length 3
def partition(lst):
    random.shuffle(lst)
    division = len(lst) / float(3)
    return [lst[int(round(division * i)): int(round(division * (i + 1)))] for i in range(3) ]

# return a list of words from text file
def get_words(d):
    f = open(d, 'r')
    l = f.read()

    wordlist = l.split()
    wordlist = [w for w in wordlist if len(w) >= 3]

    return wordlist

def get_prob(train_pos, train_neg, path):
    wordlist_pos = []
    wordlist_neg = []
    prob_pos = {}
    prob_neg = {}

    for file in train_pos:
        wordlist_pos += get_words(path + "/pos" + "/" + file)

    for file in train_neg:
        wordlist_neg += get_words(path + "/neg" + "/" + file)

    wordlist = wordlist_pos + wordlist_neg
    word_count = Counter(wordlist)

    N_pos = len(wordlist_pos)
    word_count_pos = Counter(wordlist_pos)

    N_neg = len(wordlist_neg)
    word_count_neg = Counter(wordlist_neg)

    V = len(word_count.keys()) # unique words in the training set

    for word in word_count_pos:
        prob_pos[word] = (word_count_pos[word] + 1) / (float(N_pos) + V + 1)

    # inserting probability if word is not in the training set
    prob_pos[""] = 1 / (float(N_pos) + V + 1)

    for word in word_count_neg:
        prob_neg[word] = (word_count_neg[word] + 1) / (float(N_neg) + V + 1)

    # inserting probability if word is not in the training set
    prob_neg[""] = 1 / (float(N_neg) + V + 1)

    return (prob_pos, prob_neg)

def test_prob(prob_pos, prob_neg, test_pos, test_neg, path):
    ff = filelist(path)
    pp = float(len(ff[0])) / (len(ff[0])+ len(ff[1]))
    pn = 1 - pp

    num_pos_test = 0
    num_neg_test = 0
    correct_pos_test = 0
    correct_neg_test = 0

    for file in test_neg:
        wordlist = get_words(path + "/neg" + "/" + file)
        word_count = Counter(wordlist)
        a = 0
        b = 0

        for word in word_count:
            if word not in prob_pos:
                prob_pos[word] = prob_pos[""]

            if word not in prob_neg:
                prob_neg[word] = prob_neg[""]

            a += log(prob_pos[word]) * word_count[word]
            b += log(prob_neg[word]) * word_count[word]

        p_pos = log(pp) + a
        p_neg = log(pn) + b

        if p_pos > p_neg:
            num_pos_test += 1

        else:
            num_neg_test += 1
            correct_neg_test += 1


    for file in test_pos:
        wordlist = get_words(path + "/pos" + "/" + file)
        word_count = Counter(wordlist)

        a = 0
        b = 0

        for word in word_count:
            if word not in prob_pos:
                prob_pos[word] = prob_pos[""]

            if word not in prob_neg:
                prob_neg[word] = prob_neg[""]

            a += log(prob_pos[word]) * word_count[word]
            b += log(prob_neg[word]) * word_count[word]

        p_pos = log(pp) + a
        p_neg = log(pn) + b

        if p_pos > p_neg:
            num_pos_test += 1
            correct_pos_test += 1
        else:
            num_neg_test += 1

    return (num_pos_test, num_neg_test, correct_pos_test, correct_neg_test)

def main():
    args = parseArgument()
    directory = args['d'][0]
    print directory

    path = directory

    lst_pos = filelist(path)[0] #list of files with positive review
    lst_neg = filelist(path)[1] #list of files with negative review

    pos = partition(lst_pos)
    neg = partition(lst_neg)

    mean_aver_sum = 0

    for index in itertools.combinations([0, 1, 2], 2):
        train_pos = pos[index[0]] + pos[index[1]]
        train_neg = neg[index[0]] + neg[index[1]]

        test_pos = pos[3 - sum(index)] #list of positive testing set
        test_neg = neg[3 - sum(index)] #list of negtive testing set

        a = get_prob(train_pos, train_neg, path)
        prob_pos = a[0]
        prob_neg = a[1]

        result = test_prob(prob_pos, prob_neg, test_pos, test_neg, path)

        num_pos_test_docs = len(test_pos)
        num_pos_correct_docs = result[2]

        num_neg_test_doc = len(test_neg)
        num_neg_correct_docs = result[3]

        accuracy = 100 * (num_neg_correct_docs + num_pos_correct_docs) / float((num_pos_test_docs + num_neg_test_doc))
        mean_aver_sum += accuracy

        print "iteration %d:"            % sum(index)
        print "num_pos_test_docs: %d"    % len(test_pos)
        print "num_pos_traning_docs: %d" % len(train_pos)
        print "num_pos_correct_docs: %d" % result[2]
        print "num_neg_test_docs: %d"    % len(test_neg)
        print "num_neg_traning_docs: %d" % len(train_neg)
        print "num_neg_correct_docs: %d" % result[3]
        print "accuracy: {0: .00f}%".format(accuracy)

    mean_aver = mean_aver_sum / 3
    print "ave_accuracy: {0: .1f}%". format(mean_aver)

main()

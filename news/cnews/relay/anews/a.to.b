#! /bin/sh
# a.to.b: A-format news to B-format converter (thanks, Norman)
PATH=/bin:/usr/bin:/usr/ucb; export PATH

sed '
1s/^A/Message-ID: /
2s/^/Newsgroups: /
3{
s/^/Path: /p
s/Path/From/
}
4s/^/Date: /
5{
s/^/Subject: /p
s/.*//
}
'

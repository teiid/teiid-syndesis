#!/bin/bash

exec 5<>/dev/tcp/chat.freenode.net/6667
echo "USER komodo-ci 8 * : Komodo CircleCI" >&5
echo "NICK komodo-ci" >&5
echo "PRIVMSG NickServ :IDENTIFY ${IRC_NICKSERV_PASSWORD:-2hot2work}" >&5
sleep 20
echo "JOIN #teiid" >&5

if [ -n "$CIRCLE_PR_NUMBER" ]; then
    echo "PRIVMSG #teiid :😱 PR $CIRCLE_PR_NUMBER ($CIRCLE_PR_USERNAME) - CircleCI job '$CIRCLE_JOB' failed" >&5
    sleep 2
    echo "PRIVMSG #teiid :📝 https://github.com/teiid/teiid-komodo/pull/$CIRCLE_PR_NUMBER" >&5
    sleep 2
else
    echo "PRIVMSG #teiid :😱 Master : CircleCI job '$CIRCLE_JOB' failed (last commit: $CIRCLE_USERNAME)" >&5
    sleep 2
fi

echo "PRIVMSG #teiid :🚧 $CIRCLE_BUILD_URL" >&5
echo "QUIT" >&5
cat <&5

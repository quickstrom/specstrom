DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

GITLAB_HOME=$DIR/tmp/gitlab-data
HTML_REPORT_DIR=$DIR/tmp/html-report

mkdir -p $GITLAB_HOME

if [ ! "$(docker ps | grep -q gitlab)" ]; then
  echo "GitLab already running"
else
    docker run --detach \
        --hostname localhost \
        --publish 8080:80 --publish 22:22 \
        --name gitlab \
        --rm \
        --volume $GITLAB_HOME/config:/etc/gitlab \
        --volume $GITLAB_HOME/logs:/var/log/gitlab \
        --volume $GITLAB_HOME/data:/var/opt/gitlab \
        gitlab/gitlab-ee:13.9.1-ee.0
fi


if [ ! "$(docker ps | grep -q webdriver-chrome)" ]; then
  echo "Chromedriver already running"
else
    docker run --rm -d \
        --name webdriver-chrome \
        --network=host \
        -v /dev/shm:/dev/shm \
        selenium/standalone-chrome:3.141.59-20200826
fi

rm -r $HTML_REPORT_DIR

quickstrom check --webdriver-path=/wd/hub --browser chrome gitlab.spec.purs http://localhost:8080 --reporter console --reporter html --html-report-directory $HTML_REPORT_DIR --tests=10 --max-actions=100

echo "HTML report:"
echo ""
echo "    file://$HTML_REPORT_DIR"
echo ""

GITLAB_HOME=$(mktemp -d)

docker run --detach \
  --hostname localhost \
  --publish 8080:80 --publish 22:22 \
  --name gitlab \
  --volume $GITLAB_HOME/config:/etc/gitlab \
  --volume $GITLAB_HOME/logs:/var/log/gitlab \
  --volume $GITLAB_HOME/data:/var/opt/gitlab \
  gitlab/gitlab-ee:13.9.1-ee.0

echo "Starting GPG AGENT"
echo "Start a new terminal to read the variables"
echo "The agent will stop when you close this window"
eval $(gpg-agent --daemon \
--no-detach \
--log-file "${HOME}/gpg.log" \
--write-env-file "${HOME}/.gpg-agent-info" \
--use-standard-socket)
#--enable-ssh-support \

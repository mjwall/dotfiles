if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi
# llvm from brew
export PATH="/usr/local/opt/llvm/bin:$PATH"
# added by Anaconda3 5.3.0 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
# slowness here
#__conda_setup="$(CONDA_REPORT_ERRORS=false '/Users/mjwall/anaconda3/bin/conda' shell.bash hook 2> /dev/null)"
#if [ $? -eq 0 ]; then
if [ 1 -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/mjwall/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/mjwall/anaconda3/etc/profile.d/conda.sh"
        CONDA_CHANGEPS1=false conda activate base
    else
        export PATH="/Users/mjwall/anaconda3/bin:$PATH"
    fi
fi
#unset __conda_setup
# <<< conda init <<<

#!/bin/bash
# just a thin wrapper for calling emacs

me=`realpath $0`
my_dir=`dirname $me`
if [ `which xvfb-run` ]; then
    xvfb-run emacs -Q -L . -eval "(progn (add-to-list 'load-path \"$my_dir\")(load-file \"$my_dir/publish-org-tree.el\")(publish-project)(save-buffers-kill-terminal))"
else
    emacs --batch -Q -L . -eval "(progn (add-to-list 'load-path \"$my_dir\")(load-file \"$my_dir/publish-org-tree.el\")(publish-project))"
fi

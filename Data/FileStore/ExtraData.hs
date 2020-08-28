module Data.FileStore.ExtraData where


import qualified Data.ByteString.Lazy.Char8 as B


postUpdate :: B.ByteString
postUpdate = 
  B.pack 
    "#!/bin/bash\n\
    \#\n\
    \# This hook does two things:\n\
    \#\n\
    \#  1. update the \"info\" files that allow the list of references to be\n\
    \#     queries over dumb transports such as http\n\
    \#\n\
    \#  2. if this repository looks like it is a non-bare repository, and\n\
    \#     the checked-out branch is pushed to, then update the working copy.\n\
    \#     This makes \"push\" function somewhat similarly to darcs and bzr.\n\
    \#\n\
    \# To enable this hook, make this file executable by \"chmod +x post-update\".\n\
    \\n\
    \git-update-server-info\n\
    \\n\
    \is_bare=$(git-config --get --bool core.bare)\n\
    \\n\
    \if [ -z \"$is_bare\" ]\n\
    \then\n\
    \    # for compatibility's sake, guess\n\
    \    git_dir_full=$(cd $GIT_DIR; pwd)\n\
    \    case $git_dir_full in */.git) is_bare=false;; *) is_bare=true;; esac\n\
    \fi\n\
    \\n\
    \update_wc() {\n\
    \    ref=$1\n\
    \    echo \"Push to checked out branch $ref\" >&2\n\
    \    if [ ! -f $GIT_DIR/logs/HEAD ]\n\
    \    then\n\
    \        echo \"E:push to non-bare repository requires a HEAD reflog\" >&2\n\
    \        exit 1\n\
    \    fi\n\
    \    if (cd $GIT_WORK_TREE; git-diff-files -q --exit-code >/dev/null)\n\
    \    then\n\
    \        wc_dirty=0\n\
    \    else\n\
    \        echo \"W:unstaged changes found in working copy\" >&2\n\
    \        wc_dirty=1\n\
    \        desc=\"working copy\"\n\
    \    fi\n\
    \    if git diff-index --cached HEAD@{1} >/dev/null\n\
    \    then\n\
    \        index_dirty=0\n\
    \    else\n\
    \        echo \"W:uncommitted, staged changes found\" >&2\n\
    \        index_dirty=1\n\
    \        if [ -n \"$desc\" ]\n\
    \        then\n\
    \            desc=\"$desc and index\"\n\
    \        else\n\
    \            desc=\"index\"\n\
    \        fi\n\
    \    fi\n\
    \    if [ \"$wc_dirty\" -ne 0 -o \"$index_dirty\" -ne 0 ]\n\
    \    then\n\
    \        new=$(git rev-parse HEAD)\n\
    \        echo \"W:stashing dirty $desc - see git-stash(1)\" >&2\n\
    \        ( trap 'echo trapped $$; git symbolic-ref HEAD \"'\"$ref\"'\"' 2 3 13 15 ERR EXIT\n\
    \        git-update-ref --no-deref HEAD HEAD@{1}\n\
    \        cd $GIT_WORK_TREE\n\
    \        git stash save \"dirty $desc before update to $new\";\n\
    \        git-symbolic-ref HEAD \"$ref\"\n\
    \        )\n\
    \    fi\n\
    \\n\
    \    # eye candy - show the WC updates :)\n\
    \    echo \"Updating working copy\" >&2\n\
    \    (cd $GIT_WORK_TREE\n\
    \    git-diff-index -R --name-status HEAD >&2\n\
    \    git-reset --hard HEAD)\n\
    \}\n\
    \\n\
    \if [ \"$is_bare\" = \"false\" ]\n\
    \then\n\
    \    active_branch=`git-symbolic-ref HEAD`\n\
    \    export GIT_DIR=$(cd $GIT_DIR; pwd)\n\
    \    GIT_WORK_TREE=${GIT_WORK_TREE-..}\n\
    \    for ref\n\
    \    do\n\
    \        if [ \"$ref\" = \"$active_branch\" ]\n\
    \        then\n\
    \            update_wc $ref\n\
    \        fi\n\
    \    done\n\
    \fi"

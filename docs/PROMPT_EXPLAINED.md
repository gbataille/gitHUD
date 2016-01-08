Prompt Explained
================

### When not in a git repository

The gitHUD program exists promptly and does not display anything, i.e. leaves
your prompt empty of any git-related information

## Git Repo Indicator

_Hidden by:_ `show_part_repo_indicator=False`

| Output part | Description |
|:-----------:| ----------- |
| ![repo_indicator] | This is a small prefix to the gitHUD output that indicates that you are in a git repository and that therefore gitHUD is doing some output. |

## Merge Branch information

_Hidden by:_ `show_part_merge_branch_commits_diff=False`

This section is for branches that have a remote counterpart (typically long
lived branches). It tells you the difference between your remote
tracking branch and the branch it was created from (both of which can evolve
because of you or because of your colleagues.

### No merge branch

When there is no remote merge branch, this section of the prompt is empty.
This is typically the case when you work on master, or when you work on a
local __short lived__ feature branch that has no remote counterpart.

### Merge branch

| Output part | Description | Viz |
|:-----------:| ----------- |:---:|
| ![merge_branch_pull] | This means that the remote parent of your current remote tracking branch has some commits that will need to be merged back in your branch | ![gitgraph_merge_branch_pull] |
| ![merge_branch_push] | This means that your remote tracking branch has some commits that have not yet been merged and pushed to its parent branch | ![gitgraph_merge_branch_push] |
| ![merge_branch_push_pull] | This means that both your remote tracking branch and its parent have evolved and you'll have some merge work to do | ![gitgraph_merge_branch_push_pull] |

## Local branch - detached information

_Hidden by:_ `show_part_local_branch=False`

| Output part | Description |
|:-----------:| ----------- |
| ![local_branch] | This is the name of the local branch you are working on |
| ![detached] | This is the commit sha on which you are, if you are not at the HEAD of a branch (typically on rebase situations, conflict resolution, or exploration of an old repo state) |

[repo_indicator]: ../images/prompt_repo_indicator.png
[commits_pull]: ../images/prompt_commits_pull.png
[commits_push]: ../images/prompt_commits_push.png
[commits_push_pull]: ../images/prompt_commits_push_pull.png
[conflicts]: ../images/prompt_conflicts.png
[detached]: ../images/prompt_detached.png
[local_branch]: ../images/prompt_local_branch.png
[merge_branch_pull]: ../images/prompt_merge_branch_pull.png
[merge_branch_push]: ../images/prompt_merge_branch_push.png
[repo_changes]: ../images/prompt_repo_changes.png
[repo_indicator]: ../images/prompt_repo_indicator.png
[stash]: ../images/prompt_stash.png
[merge_branch_push_pull]: ../images/prompt_merge_branch_push_pull.png

[gitgraph_merge_branch_pull]: ../images/gitgraph_merge_branch_pull.png
[gitgraph_merge_branch_push]: ../images/gitgraph_merge_branch_push.png
[gitgraph_merge_branch_push_pull]: ../images/gitgraph_merge_branch_push_pull.png

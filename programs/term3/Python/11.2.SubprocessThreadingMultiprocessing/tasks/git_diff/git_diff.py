import subprocess
from pathlib import Path


def get_changed_dirs(git_path: Path, from_commit_hash: str, to_commit_hash: str) -> set[Path]:
    """
    Get directories which content was changed between two specified commits
    :param git_path: path to git repo directory
    :param from_commit_hash: hash of commit to do diff from
    :param to_commit_hash: hash of commit to do diff to
    :return: sequence of changed directories between specified commits
    """
    proc = subprocess.Popen(['git', 'diff', from_commit_hash, to_commit_hash], cwd=git_path, stdout=subprocess.PIPE)
    out, _ = proc.communicate()
    res = set()
    for s in out.decode().split('\n'):
        if s.startswith('+++') or s.startswith('---'):
            res.add(git_path / Path(s.split(' ')[1].split('/')[-2]))
    return res

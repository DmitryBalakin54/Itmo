from _pytest.terminal import TerminalReporter, WarningReport
import pytest


@pytest.hookimpl(tryfirst=True)
def pytest_terminal_summary(terminalreporter: TerminalReporter) -> None:
    """ Adding custom section in pytest summary """
    if terminalreporter.config.getoption("collectonly"):
        return

    terminalreporter.section("teardown summaries")

    teardown_summaries = []
    for reps in terminalreporter.stats.values():
        for rep in reps:
            if isinstance(rep, WarningReport):
                continue

            if rep.when == 'teardown':

                for secname, content in rep.sections:
                    if 'teardown' in secname:
                        teardown_summaries.append(content)

    terminalreporter.write(''.join(teardown_summaries))
    terminalreporter.currentfspath = 1
    terminalreporter.ensure_newline()

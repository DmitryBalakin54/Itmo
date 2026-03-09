r(ContestId) :-
    Sessions(SessionId, TeamId, ContestId, _),
    Teams(TeamId, _, UnivId),
    Universities(UnivId, :UnivName).
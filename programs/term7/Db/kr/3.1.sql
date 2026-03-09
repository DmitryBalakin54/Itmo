delete
from Runs as r
where r.SessionId in (
    select s.SessionId
    from Teams as t
    natural join Sessions as s
    where t.TeamName = :TeamName
);

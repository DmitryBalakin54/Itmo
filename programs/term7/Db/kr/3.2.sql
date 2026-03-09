delete
from Runs as r
where r.SessionId in (
    select s.SessionId
    from Contests as c
    natural join Sessions as s
    where c.ContestName = :ContestName
);

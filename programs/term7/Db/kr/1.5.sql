select t.TeamName
from Teams as t
where not exists (
    select 1
    from Runs as r
    natural join Sessions as s
    where r.Accepted = 1
    and s.TeamId = t.TeamId
);

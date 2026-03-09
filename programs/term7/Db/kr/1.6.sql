select distinct p.ProblemName, p.ContestId
from Problems as p
where not exists (
    select 1
    from Problems as pp
    natural join Runs as r
    where r.Accepted = 1
    and p.Letter = pp.Letter
    and p.ContestId = pp.ContestId
);
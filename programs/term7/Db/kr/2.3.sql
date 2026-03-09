select p.ContestId, p.Letter
from Problems as p
where not exists (
    select 1
    from Sessions as s
    natural join Runs as r
    where r.Accepted = 1
    and p.Letter = r.Letter 
    and s.ContestId = p.ContestId
);

select r.RunId, s.TeamId, r.SubmitTime, r.Accepted
from Runs as r
natural join Sessions as s
natural join Contests as c
where c.ContestName = :ContestName
and r.Letter = :Letter;

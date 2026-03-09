select r.RunId, r.SessionId, r.Letter, r.SubmitTime
from Runs as r
natural join Sessions as s
natural join Contests as c
where r.Accepted = 0
and c.ContestName = :ContestName;

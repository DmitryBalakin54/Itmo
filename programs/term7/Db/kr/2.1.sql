select distinct s.TeamId
from Sessions as s
natural join Runs as r
where r.Accepted = 1
and s.ContestId = :ContestId
and r.Letter = :Letter;

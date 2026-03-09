select r.RunId, r.SessionId, r.Letter, r.SubmitTime, r.Accepted 
from Runs as r
join Sessions as s on s.SessionId = r.SessionId
join Teams as t on t.TeamId = s.TeamId
join Contests as c on c.ContestId = s.ContestId
where t.TeamName = :TeamName
and c.ContestName = :ContestName;
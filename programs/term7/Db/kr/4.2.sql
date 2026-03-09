select t.TeamId, COUNT(distinct r.Letter) as Opened
from Teams as t
left join Sessions as s on t.TeamId = s.TeamId
left join Runs as r on r.SessionId = s.SessionId
group by t.TeamId;
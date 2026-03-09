select r.SessionId, COUNT(distinct r.Letter) as Opened
from Runs as r
group by r.SessionId;
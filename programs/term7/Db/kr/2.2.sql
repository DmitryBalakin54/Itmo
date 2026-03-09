select distinct s.ContestId
from Sessions as s 
natural join Teams as t
natural join Universities as u 
where u.UnivName = :UnivName;

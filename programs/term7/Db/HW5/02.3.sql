SELECT StudentId, StudentName, GroupName 
FROM Students NATURAL JOIN Groups
WHERE GroupName = :GroupName
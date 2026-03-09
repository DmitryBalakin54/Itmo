SELECT StudentId, StudentName, GroupName 
FROM (SELECT * 
    FROM Students
    WHERE StudentId = :StudentId
) AS S NATURAL JOIN Groups
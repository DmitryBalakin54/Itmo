SELECT DISTINCT StudentId, StudentName, GroupName
FROM Clubs
LEFT JOIN (
    SELECT StudentId AS ClubStudentHeadId, StudentName AS HeadName
    FROM Students 
) AS S ON S.ClubStudentHeadId = Clubs.ClubStudentHeadId
NATURAL JOIN ClubMembers
NATURAL JOIN Students 
NATURAL JOIN Groups
WHERE HeadName = :StudentName
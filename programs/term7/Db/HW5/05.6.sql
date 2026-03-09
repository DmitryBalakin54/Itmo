SELECT StudentName1, StudentName2, ClubName
FROM (
    SELECT StudentId1, StudentId2, ClubId
    FROM (
        SELECT StudentId AS StudentId1, ClubId
        FROM ClubMembers
    ) AS S1
    NATURAL JOIN (
        SELECT StudentId AS StudentId2, ClubId
        FROM ClubMembers
    ) AS S2
WHERE S1.StudentId1 < S2.StudentId2
) AS PP
NATURAL JOIN (
    SELECT StudentId AS StudentId1, StudentName AS StudentName1
    FROM Students
) AS SS1
NATURAL JOIN (
    SELECT StudentId AS StudentId2, StudentName AS StudentName2
    FROM Students
) AS SS2
NATURAL JOIN Clubs
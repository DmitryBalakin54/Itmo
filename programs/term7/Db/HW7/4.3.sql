UPDATE Students AS S
SET Marks = (
    SELECT COUNT(*)
    FROM Marks AS M
    WHERE M.StudentId = S.StudentId
)
WHERE S.StudentId IN (
    SELECT CM.StudentId
    FROM ClubMembers AS CM
    WHERE CM.ClubId IN (
        SELECT C.ClubId
        FROM Clubs AS C
        WHERE C.ClubName = :ClubName
    )
)
DELETE FROM Students
WHERE StudentId IN (
    SELECT StudentId
    FROM ClubMembers
    WHERE ClubId IN (
        SELECT ClubId
        FROM Clubs
        WHERE ClubName = :ClubName
    )
)
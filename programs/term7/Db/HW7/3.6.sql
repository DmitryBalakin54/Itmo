UPDATE Students
SET GroupId = (
    SELECT GroupId
    FROM Groups
    WHERE GroupName = :GroupName
)
WHERE EXISTS (
    SELECT GroupId
    FROM Groups
    WHERE GroupName = :GroupName
) 
AND GroupId IN (
    SELECT GroupId
    FROM Groups
    WHERE GroupName = :FromGroupName
) 
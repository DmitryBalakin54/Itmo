WITH StudentCourses AS (
    SELECT S.StudentId, P.CourseId
    FROM Students AS S, Plan AS P
    WHERE S.GroupId = P.GroupId

    UNION

    SELECT M.StudentId, M.CourseId
    FROM Marks AS M
)

SELECT S1.StudentName AS StudentName1,
S2.StudentName AS StudentName2,
C.CourseName AS CourseName
FROM Students AS S1,
Students AS S2,
Courses AS C,
ClubMembers AS CM1,
ClubMembers AS CM2,
StudentCourses AS SC1,
StudentCourses AS SC2
WHERE SC1.StudentId = S1.StudentId
AND SC2.StudentId = S2.StudentId
AND SC1.CourseId = C.CourseId
AND SC2.CourseId = C.CourseId
AND CM1.StudentId = S1.StudentId
AND CM2.StudentId = S2.StudentId
AND CM1.ClubId = CM2.ClubId
AND S1.StudentId < S2.StudentId
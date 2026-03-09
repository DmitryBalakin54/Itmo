CREATE UNIQUE INDEX ind_Groups ON Groups USING HASH (GroupId);

CREATE INDEX ind_Groups_GroupName ON Groups USING BTREE (GroupName, GroupId);

CREATE UNIQUE INDEX ind_Students ON Students USING HASH (StudentId);

CREATE INDEX ind_Students_StudentName ON Students USING BTREE (StudentName, StudentId);

CREATE INDEX ind_Students_GroupId ON Students USING HASH (GroupId);

CREATE UNIQUE INDEX ind_Courses ON Courses USING HASH (CourseId);

CREATE INDEX ind_Courses_CourseName ON Courses USING BTREE (CourseName, CourseId);

CREATE UNIQUE INDEX ind_Lecturers ON Lecturers USING HASH (LecturerId);

CREATE INDEX ind_Lecturers_LecturerName ON Lecturers USING BTREE (LecturerName, LecturerId);

CREATE INDEX ind_Plan_GroupId ON Plan USING BTREE (GroupId, CourseId, LecturerId);

CREATE INDEX ind_Plan_CourseId ON Plan USING BTREE (LecturerId, CourseId);

CREATE INDEX ind_Marks_StudentId ON Marks USING BTREE (StudentId, CourseId, Mark);

CREATE INDEX ind_Marks_CourseId ON Marks USING BTREE (CourseId, StudentId, Mark);

CREATE UNIQUE INDEX ind_Clubs ON Clubs USING HASH (ClubId);

CREATE INDEX ind_Clubs_ClubName ON Clubs USING BTREE (ClubName, ClubId);

CREATE INDEX ind_Clubs_ClubStudentHeadId ON Clubs USING BTREE (ClubStudentHeadId, ClubId);

CREATE INDEX ind_ClubMembers_ClubId ON ClubMembers USING BTREE (ClubId);


SELECT AVG(M.Mark) AS AvgMark
FROM Clubs AS CB
NATURAL JOIN ClubMembers AS CM 
NATURAL JOIN Marks AS M
NATURAL JOIN Courses AS C
WHERE CB.ClubName = :ClubName
AND C.CourseName = :CourseName;

CREATE INDEX ind_Clubs_ClubName ON Clubs USING BTREE (ClubName, ClubId);

CREATE INDEX ind_ClubMembers ON ClubMembers USING BTREE (ClubId, StudentId);

CREATE INDEX ind_Marks_StudentId ON Marks USING BTREE (StudentId, CourseId, Mark);

CREATE INDEX ind_Courses_CourseName ON Courses USING BTREE (CourseName, CourseId);


SELECT COUNT(Mark), StudentId
FROM Marks AS M
WHERE Mark < 3
GROUP BY StudentId;

CREATE INDEX ind_Marks_Mark ON Marks USING BTREE (Mark, StudentId);

SELECT StudentId
FROM Students
NATURAL JOIN Groups
WHERE GroupName like 'M34%';

CREATE INDEX ind_Groups_GroupName ON Groups USING BTREE (GroupName);

CREATE INDEX ind_Students_GroupId ON Students USING BTREE (GroupId, StudetId);

SELECT StudentId
FROM ClubMembers
GROUP BY StudentId
HAVING COUNT(ClubId) >= 2;

CREATE INDEX ind_ClubMembers_StudentId ON ClubMembers USING BTREE (StudentId, ClubId);
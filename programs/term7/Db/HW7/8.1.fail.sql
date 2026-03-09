TRUNCATE Marks, Plan, Students, Groups, Lecturers, Courses RESTART IDENTITY CASCADE;

INSERT INTO Groups (GroupId, GroupName) VALUES
(1, 'G1'),
(2, 'G2');

INSERT INTO Students (StudentId, StudentName, GroupId) VALUES
(1, 'Иванов', 1),
(2, 'Петров', 1),
(3, 'Сидоров', 2);

INSERT INTO Lecturers (LecturerId, LecturerName) VALUES
(1, 'Лектор А'),
(2, 'Лектор Б');

INSERT INTO Courses (CourseId, CourseName) VALUES
(101, 'Математика'),
(102, 'Физика'),
(103, 'Информатика'),
(104, 'История');

INSERT INTO Plan (GroupId, CourseId, LecturerId) VALUES
(1, 101, 1),
(1, 102, 2),
(2, 103, 1),
(2, 104, 2);

INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (1, 101, 90);
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (3, 103, 95);

BEGIN;

SAVEPOINT sp1;
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (1, 103, 70);
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp1;

SAVEPOINT sp2;
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (3, 102, 80);
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp2;

SAVEPOINT sp3;
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (2, 104, 75);
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp3;

SAVEPOINT sp4;
UPDATE Students SET GroupId = 2 WHERE StudentId = 1;
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp4;

SAVEPOINT sp5;
UPDATE Plan SET GroupId = 1 WHERE GroupId = 2 AND CourseId = 103;
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp5;

SAVEPOINT sp6;
DELETE FROM Plan WHERE GroupId = 1 AND CourseId = 101;
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp6;

SAVEPOINT sp7;
UPDATE Students SET StudentId = 10 WHERE StudentId = 1;
EXCEPTION WHEN OTHERS THEN
  ROLLBACK TO SAVEPOINT sp7;

COMMIT;

SELECT * FROM Marks;

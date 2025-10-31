-- Seed data for Faculty Network Services Information System

USE faculty_services;

-- Insert Authors
INSERT INTO authors (name, email, department) VALUES
('Ivan Petrenko', 'ivan.petrenko@university.edu', 'Computer Science Department'),
('Maria Kovalenko', 'maria.kovalenko@university.edu', 'Mathematics Department'),
('Oleksandr Sydorenko', 'olexandr.sydorenko@university.edu', 'Physics Department');

-- Insert Service Types
INSERT INTO service_types (name, description) VALUES
('Educational Service', 'Services for the educational process'),
('Research Service', 'Services for scientific research'),
('Administrative Service', 'Services for administration'),
('Information Service', 'Services for information distribution');

-- Insert Services
INSERT INTO services (name, author_id, annotation, service_type_id) VALUES
('Course Management System', 1, 'Web platform for managing educational courses and materials', 1),
('Research Publications Database', 2, 'Centralized database of faculty scientific articles', 2),
('Faculty Portal', 3, 'Main information portal of the faculty with news and announcements', 4);

-- Insert Versions
INSERT INTO versions (service_id, version_number, release_date, is_current) VALUES
(1, '1.0.0', '2024-01-15', TRUE),
(1, '1.1.0', '2024-03-20', FALSE),
(2, '2.0.0', '2024-02-10', TRUE),
(3, '1.5.0', '2024-01-05', TRUE);

-- Insert Terms and Conditions
INSERT INTO terms_conditions (service_id, rules, usage_conditions, deadline) VALUES
(1, 'Users have the right to view assigned courses. Distribution of materials without permission is prohibited.', 'Registration is required. Access is granted to students and teachers.', '2024-12-31'),
(2, 'Publications are available only for scientific purposes. Citation is required when using.', 'Access for faculty researchers. Registration through university email.', '2025-06-30'),
(3, 'Only authorized users can publish announcements. Inappropriate content will be removed.', 'Free access for all faculty network users.', NULL);

-- Insert System Users
INSERT INTO system_users (username, full_name, email, role) VALUES
('student001', 'Andrii Melnyk', 'andrii.melnyk@student.university.edu', 'student'),
('teacher001', 'Professor Volodymyr Demchenko', 'v.demchenko@university.edu', 'teacher'),
('admin001', 'System Administrator', 'admin@university.edu', 'admin');

-- Insert User Registrations
INSERT INTO user_registrations (user_id, service_id, registration_date, status) VALUES
(1, 1, '2024-01-20 10:00:00', 'active'),
(1, 3, '2024-01-18 14:30:00', 'active'),
(2, 1, '2024-01-16 09:00:00', 'active'),
(2, 2, '2024-02-15 11:00:00', 'active'),
(3, 1, '2024-01-10 08:00:00', 'active'),
(3, 2, '2024-02-10 10:00:00', 'active'),
(3, 3, '2024-01-05 12:00:00', 'active');

-- Insert Usage Statistics
INSERT INTO usage_statistics (service_id, user_id, access_date, duration_minutes, action_type) VALUES
(1, 1, '2024-03-01 09:00:00', 45, 'view_courses'),
(1, 1, '2024-03-02 14:20:00', 30, 'download_material'),
(1, 2, '2024-03-01 10:15:00', 120, 'create_course'),
(2, 2, '2024-03-03 11:30:00', 60, 'search_publications'),
(3, 1, '2024-03-01 08:00:00', 10, 'view_announcements'),
(3, 3, '2024-03-01 16:00:00', 25, 'publish_announcement');



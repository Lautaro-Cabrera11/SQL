DROP SCHEMA IF EXISTS Tablanueva;
CREATE SCHEMA IF NOT EXISTS Tablanueva;
USE Tablanueva;
CREATE TABLE Cliente (
    IDCliente INT PRIMARY KEY,
    Nombre VARCHAR(255)
);
CREATE TABLE Auto (
IDCliente INT  PRIMARY KEY,
    VIN VARCHAR(17) not null,
    KILOMETROS VARCHAR(255),
    Matricula VARCHAR(100)
);
CREATE INDEX idx_matricula ON Auto(Matricula);
DROP table  IF EXISTS Servicio_Garantia;
CREATE TABLE Servicio_Garantia (
    IDServicio_Cliente INT auto_increment PRIMARY KEY,
    Detalle_Averia VARCHAR(500),
    Fecha_Apertura DATE,
    Matricula VARCHAR(100)
);

CREATE TABLE Piezas_Cambiadas (
    IDPiezas_Cambiadas INT PRIMARY KEY,
    Articulo VARCHAR(100),
    Denominacion_Articulo VARCHAR(255),
    Matricula VARCHAR(100)
);



/* Funciones*/
DROP FUNCTION IF EXISTS Pieza_Cambiada;
DELIMITER $$
CREATE FUNCTION Pieza_Cambiada(Patente VARCHAR(100))
RETURNS VARCHAR(255)
DETERMINISTIC
BEGIN
    DECLARE Tipo_Pieza VARCHAR(255);

    SELECT DISTINCT Denominacion_Articulo
    INTO Tipo_Pieza
    FROM Piezas_Cambiadas
    WHERE Matricula = Patente;

    RETURN Tipo_Pieza;
END$$
DELIMITER ;

SELECT Pieza_Cambiada('AF058DL') AS RESULTADO;

DROP FUNCTION IF EXISTS Kilometraje_Promedio_Por_Patente;
DELIMITER $$
CREATE FUNCTION Kilometraje_Promedio_Por_Patente(Matricula VARCHAR(100)) 
RETURNS DECIMAL(10,2)
DETERMINISTIC
READS SQL DATA
BEGIN
    DECLARE promedio DECIMAL(10,2);
    SELECT AVG(CONVERT(KILOMETROS, DECIMAL(10,2)))
    INTO promedio
    FROM Auto
    WHERE Matricula = Matricula;
    RETURN promedio;
END$$
DELIMITER ;
SELECT Kilometraje_Promedio_Por_Patente('AF058DL') AS RESULTADO;

/* STORE PROCEDURE 
Select * from auto
ORDER BY IDCliente Desc;
Select * from auto
ORDER BY Pd_Columna PD_Orden; */
/*Idea*/

Drop Procedure if Exists OrdenarTablaAuto;
DELIMITER //
CREATE PROCEDURE OrdenarTablaAuto(
    IN columna_nombre VARCHAR(100), 
    IN ordenamiento VARCHAR(100)
)
BEGIN
    SET @sql_query = CONCAT('SELECT * FROM Auto ORDER BY ', columna_nombre, ' ', ordenamiento);
    PREPARE statement FROM @sql_query;
    EXECUTE statement;
    DEALLOCATE PREPARE statement;
END //
DELIMITER ;
CALL OrdenarTablaAuto('IDCliente', 'DESC');
/* 2do Caso */

Drop Procedure if Exists InsertarElimnarPiezasCambiadas;
Delimiter //
CREATE PROCEDURE InsertarElimnarPiezasCambiadas(
	IN opcion INT,
    IN id INT,
    IN articulo VARCHAR(100),
    IN denominacion_ariculo VARCHAR (100),
    IN matricula_auto VARCHAR(10)
    )
BEGIN
	IF Opcion = 1 THEN 
		INSERT INTO Piezas_Cambiadas(IDPiezas_Cambiadas,Articulo,Denominacion_Articulo,Matricula) VALUES (id,articulo,denominacion_ariculo,matricula_auto);
	ELSEIF Opcion = 2 THEN
		DELETE FROM Piezas_Cambiadas WHERE IDPiezas_Cambiadas=id;
	END IF;
    END //
 Delimiter //   
 CALL InsertarElimnarPiezasCambiadas(1, 292, '244106790R
', 'Bateria', 'AE919UF');


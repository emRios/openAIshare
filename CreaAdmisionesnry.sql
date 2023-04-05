USE [HOSPITAL]
GO

/****** Object:  StoredProcedure [dbo].[SpHisAdmisionNueva]    Script Date: 3/04/2023 10:30:55 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[SpHisAdmisionNueva](
	-- PARAMETROS DEFECTO 
	-- ========================================================== 
	@IOpcion VARCHAR(1) = '',   --Tipo de Admisión
	-- I = Admisión Interna
	-- E = Admisión Externa
	-- U = Admisión Emergencia
	-- R = Admisión Recien Nacido
	-- Q = Admisión de Paquete
	@IEmpresaU VARCHAR(3) = '',
	@IEmpresaReal VARCHAR(3) = '',
	@IHospital VARCHAR(3) = '',
	@ICodigoPac INT = 0,
	@ICliente VARCHAR(15) = '',
	@INombreFac VARCHAR(60) = '',
	@IDirecFac VARCHAR(80) = '', 
    @ITipoDoc VARCHAR(1) = '',	-- N = Nit, D = DPI, P = Pasaporte
	@IDocumento VARCHAR(15) = '',
	@DPI VARCHAR(15) = '',
	@NIT VARCHAR(15) = '',
	@Pasaporte VARCHAR(15) = '',
	@ICorporativo INT = 0,
	@ISeguro VARCHAR(3) = '', 
	@IAfiliado VARCHAR(17) = '',
	@IMedaxABC BIT = 0,
	@IMedico INT = 1,
	@IHabitacion VARCHAR(4) = NULL,  --Si TA es Interno, Emergencia o RN
	@IEdad TINYINT=0, 
	@IEdadMedida VARCHAR(1)='',
	@ISerieAdmRaiz VARCHAR(3) = '',
	@IAdmisionRaiz INT = 0,
	@ITipoPaquete SmallInt = NULL,--*** -- Si TA=PQT(Q)  Tipo de Admision (1) Interna / (2) Emergencia no se permiten otros
	@IPQTPacienteID INT = 0,--***
	@ICovid Bit = NULL,
	@IVacuna1 INT = 0,--***
	@IVacuna2 INT = 0,--***
	@IVacuna3 INT = 0,--***
	@IDiagnosticos VARCHAR(100) = '',--***  -- Códigos de PlanesMedicos..diagnosticos separados por "|"
	-- Si es RN ver si tiene paquete
	-- Puede traer ya un paquete
	-- debe venir definidas estos proximos 4 parametros externos si lo tiene
	@IPaqueteBebe INT=0, 
    @IMedaxBebe BIT = NULL,
    @IHospitalBebeCodigoBase VARCHAR(3) = NULL,--***
    @IPrecioPaqueteBebe MONEY = NULL,--***
	@IAdmisionPorHospital INT = 1 --***-- Parametro para generar series de admisión por cada hospital o unificarlas como indica el query	
)
AS
BEGIN
	DECLARE
		@ErrRaise VARCHAR(MAX) = '',
		@ErrRaiseNum INT = 0

	DECLARE
		@SerieAdmision VARCHAR(1) = '',
		@Admision INT=0,
		@NivelPrecios SmallInt = NULL,
		@PolizaPlanDirecto INT=0,
		@Paciente INT,
		@Nacimiento date,
		@Producto VARCHAR(12),
		@CodigoBase VARCHAR(3) = '', -- Para el PQTPaciente 
		@PaqueteQx BIT,
		@Interno VARCHAR(1)=NULL,
		@TipoDescuento VARCHAR(1) = 'N',
		@PlanMedax BIT,
		@Usuario VARCHAR(5),
		@CodigoT INT,   -- Codigo max de traslados
		@TipoOrden VARCHAR(3),
		@Orden INT,
		@CodigoHab VARCHAR(1),
		@Categoria VARCHAR(2),
		@Costo MONEY,
	    @Precio MONEY,
		@UMedida VARCHAR(8),
		@Factor SMALLMONEY,
		@EmerOrden INT,
		@Contador INT,
		@Registros INT,
		@Linea INT,
		@CodigoDX VARCHAR(5),
		@VACUNA INT
		/*@NIT VARCHAR(10),
		@DPI VARCHAR(20),
		@PASAPORTE VARCHAR(20)*/
		
SET NOCOUNT ON
BEGIN TRY

	Begin --Sección general

		-- Averiguando la serieadmisión y nivel de precios
		-- La serie es determinada por el hospital y IAdmisionPorHospital 1 indica que cada hospital 
		-- tiene su diferente letra, 0 indica que se usará la misma letra para todos los hospitales
		-- El nivel de precios se calcula con base al nivel en emphospital
		Select @CodigoBase = CodigoBase, @CodigoHab = CodigoHab,
		@SerieAdmision= (SELECT CASE WHEN (@IOpcion='I' OR @IOpcion='R' OR @ITipoPaquete=1) THEN Internos
											   WHEN  @IOpcion='E' THEN Externos
											   WHEN (@IOpcion='U' OR @ITipoPaquete=2) THEN Urgencias
											   ELSE Externos END
									FROM NOMINADB..EmpHospital (nolock) 
									WHERE Empresa=@IEmpresaU
							        AND  (Codigo=@IHospital AND @IAdmisionPorHospital = 1  )
									UNION ALL
									SELECT CASE WHEN (@IOpcion='I' OR @IOpcion='R' OR @ITipoPaquete=1) THEN 'I'
											   WHEN  @IOpcion='E' THEN 'E'
											   WHEN (@IOpcion='U' OR @ITipoPaquete=2) THEN 'U'
											   ELSE 'E' END
									WHERE @IAdmisionPorHospital = 0),
		@NivelPrecios = CASE WHEN (@IOpcion='I' or @IOpcion='R' OR @ITipoPaquete=1) THEN Nivel_Internos 
							 WHEN  @IOpcion='E' THEN Nivel_Externos
							 WHEN (@IOpcion='U' OR @ITipoPaquete=2) THEN Nivel_Emergencia 
							 ELSE Nivel_Externos END
		FROM NOMINADB..EmpHospital (nolock) 
		WHERE Empresa=@IEmpresaU and Codigo=@IHospital
		
		-- Si es de seguro se sobreescribe el nivelprecios con el nivel en Aseguradoras
		If @ISeguro<>''
		BEGIN
		   SELECT @NivelPrecios = CASE WHEN (@IOpcion='I' OR @IOpcion='R' OR @ITipoPaquete=1) THEN Aseguradoras.NivelPreciosInt 
									   WHEN  @IOpcion='E' THEN Aseguradoras.NivelPreciosExt
									   WHEN (@IOpcion='U' OR @ITipoPaquete=2) THEN Aseguradoras.NivelPreciosEme
									   ELSE Aseguradoras.NivelPreciosExt END
		   FROM   Seguros (nolock) LEFT OUTER JOIN
                  Aseguradoras (nolock) ON Seguros.Asegura = Aseguradoras.Asegura AND Seguros.Empresa = Aseguradoras.Empresa
		   WHERE     (Seguros.Empresa = @IEmpresaU) AND (Seguros.Codigo = @ISeguro)
		end

        -- Lee el siguiente correlativo del tipo de admision
		Select @Admision = Siguiente 
		from Correlativo_Admision (nolock)
		Where Empresa = @IEmpresaU
		AND Hospital = @IHospital
		AND Tipo = @SerieAdmision
		
		IF RTRIM(@IHabitacion)='' SET @IHabitacion=NULL
		IF @IOpcion = 'Q' SET @PaqueteQx=1 ELSE SET @PaqueteQx=0  -- Paquete Quirúrgico
		IF @IOpcion='I' OR @IOpcion='R' OR @ITipoPaquete=1 SET @Interno='S' ELSE SET @Interno='N'

		--Con Afiliado  TipoDescuento := 'S';
		--ABC		    TipoDescuento := 'N';	
		--Para lo demas TipoDescuento := 'N';
		IF @IAfiliado<>'' SET @TipoDescuento='S' ELSE SET @TipoDescuento='N'
		IF (@IAfiliado = '' AND	ISNULL(@IMedaxABC,'') = '') BEGIN SET @PlanMedax=0 SET @TipoDescuento='N' END ELSE BEGIN SET @PlanMedax=1 END
		SELECT @Usuario = LEFT(CAST(@ICorporativo AS VARCHAR(15)), 5)
		
		IF @ITipoDoc = 'N'	-- N = Nit, D = DPI, P = Pasaporte
		   SET @NIT=@IDocumento
		ELSE
		   SET @NIT=NULL

		IF @ITipoDoc = 'D'
		   SET @DPI=@IDocumento
		ELSE
		   SET @DPI=NULL

		IF @ITipoDoc = 'P'
		   SET @PASAPORTE=@IDocumento
		ELSE
		   SET @PASAPORTE=NULL

		----------  > INSERTA LA ADMISIÓN  < -------------
		Insert Into Hospital..Admisiones (Empresa, EmpresaReal, Serie, Codigo, Paciente, NivelPrecios, Usuario, 
		Status, Habitacion, Edad, EdadMedida, PaqueteQx, Interno, NombreFactura, 
		DireccionFactura, Medico, TipoDescuento, SerieAdmisionRaiz, AdmisionRaiz, Seguro, 
		PlanMedax, IdAfiliado, MedaxABC, DiagnosticoCOVID, Corporativo, Nit, Dpi, Pasaporte) 
		Values 
        (@IEmpresaU, @IEmpresaReal, @SerieAdmision, @Admision, @ICodigoPac, @NivelPrecios, @Usuario,
         'A', @IHabitacion, @IEdad, @IEdadMedida, @PaqueteQx, @Interno, @INombreFac,
		 @IDirecFac, @IMedico, @TipoDescuento, @ISerieAdmRaiz, @IAdmisionRaiz, @ISeguro,
		 @PlanMedax, @IAfiliado, @IMedaxABC, @ICovid, @ICorporativo, @NIT, @DPI, @PASAPORTE)

		 ----------  >  Aumenta el correlativo de la admisión  < -------------
		 Update Hospital..Correlativo_Admision 
		        SET Siguiente = @Admision + 1
				Where Empresa = @IEmpresaU	AND Hospital = @IHospital	AND Tipo = @SerieAdmision

		---------- >  Todo lo relacionado a pacientes internados  < ------------- 
		If ISNULL(@IHabitacion,'')<>'' AND (@IOpcion = 'I' OR @IOpcion = 'R'  OR @IOpcion = 'U' OR @ITipoPaquete=1) 
        Begin
		    -- Inserta traslado y Ocupa la habitación
			Select @CodigoT=max(codigo)+1 From Hospital..traslados (nolock) Where empresa = @IEmpresaU

			Insert into Hospital..Traslados (Empresa, Codigo, Fecha, SerieAdmision, Admision, 
			         HabitacionAnterior, HabitacionNueva, Usuario, FechaRegistro, Corporativo) 
					 Values  (@IEmpresaU , @CodigoT, getdate(), @SerieAdmision, @Admision, '', @IHabitacion,
					 @Usuario, getdate() , @ICorporativo) 
               
			-- Procederemos a registrar el cargo de la habitacion...
			SET @TipoOrden = 'AD'+ @SerieAdmision   -- Hospitalizados Internos

			-- Recuperar la categoria del producto habitacion a cargar
			SELECT @Producto=Habitaciones.Producto 
			FROM      HOSPITAL..Habitaciones (nolock) LEFT OUTER JOIN
						HOSPITAL..Areas_Habitaciones (nolock) ON (Habitaciones.Area = Areas_Habitaciones.Codigo AND
						Habitaciones.Empresa = Areas_Habitaciones.Empresa)
			WHERE     (Habitaciones.Empresa = @IEmpresaU) AND Habitaciones.Codigo = @IHabitacion
			and (Habitaciones.Status = 'D')

			If (SELECT ISNULL(@Producto, ''))='' SET @ErrRaise=@ErrRaise + 'Error: No se pudo obtener el producto de la habitación|' 

			Update Hospital..Habitaciones SET Status = 'A'
			Where Empresa = @IEmpresaU and Codigo = @IHabitacion
			
			Select @Categoria=Categoria, @Costo=Case when Costeo='P' then CostoPromedio else CostoUltimo end
			from INVENTARIO..PRODUCTOS (nolock)
			Where Empresa = @IEmpresaU
			and Codigo = @Producto

            --Recuperar el precio de la habitación del nivel de la admision

			Select @Precio=Precio, @UMedida=UnidadMedida, @Factor=Factor
			from Inventario..ProductosPrecios (nolock)
			Where Empresa = @IEmpresaU
			and SubNivel = 1  and Nivel = @NivelPrecios
			and Producto = @Producto

            -- Insertar Orden en la tabla Ordenes
            INSERT INTO Hospital..Ordenes(Empresa, Tipo, Codigo, Fecha, Interno, SerieAdmision, 
						Admision, Usuario, Status, EmpresaReal, EmpresaUnif, Habitacion) 
            VALUES (@IEmpresaU, @TipoOrden, @Admision, getdate(), 'S', @SerieAdmision,
			@Admision, @Usuario, 'A', @IEmpresaReal, @IEmpresaU, @IHabitacion )

			-- Inserta el Cargo
            INSERT INTO Hospital..Cargos (Empresa, TipoOrden, Orden, Linea, SerieAdmision, Admision, Fecha,
								Producto, Categoria, Valor, Cantidad, Status, Paciente, PrecioUnitario, 
								SubNivelPrecios, Factor, Costo, UnidadMedida, EmpresaReal, EmpresaUnif, CostoHospital) 
						Values (@IEmpresaU, @TipoOrden, @Admision, 1, @SerieAdmision, @Admision, Getdate(),
								@Producto, @Categoria, @Precio, 1, 'H', @ICodigoPac, @Precio, 1, @Factor,
								@Costo, @UMedida, @IEmpresaReal, @IEmpresaU, @Costo )


			-- Agregar diagnósticos de ingreso
			DECLARE @TablaDiag TABLE (Numero int, CodigoDx varchar(5))
			INSERT INTO @TablaDiag
			SELECT * FROM Hospital.dbo.FnStringSplit(@IDiagnosticos, '|')

			SELECT @REGISTROS = COUNT(*) FROM @TablaDiag
			SET @CONTADOR=1
			SET @CodigoDX=''
			IF (@REGISTROS>0) AND ((SELECT CodigoDx from @TablaDiag where Numero=@Contador)<>'') 
			BEGIN                  
				WHILE @CONTADOR <= @REGISTROS
				BEGIN
					SET @CodigoDX = (SELECT CodigoDx from @TablaDiag where Numero=@Contador)			

                    INSERT INTO Hospital..DiagnosticosXAdmision(Empresa, SerieAdmision, Admision, CodDiagnosticoCID10, 
														Fecha, Usuario, TipoDiagnostico, CodigoDiagnostico) 
												VALUES (@IEmpresaU, @SerieAdmision, @Admision, @CodigoDX, 
														Getdate(), @Usuario, 'I', (Select isnull((select max(CodigoDiagnostico) + 1
																					from Hospital..DiagnosticosXAdmision
																					where Empresa=@IEmpresaU AND
																					SerieAdmision=@SerieAdmision AND
																					Admision=@Admision AND
																					TipoDiagnostico='I'),1)  ) )
                SET @CONTADOR = @CONTADOR + 1;
				END -- fin del while
			END -- IF (@REGISTROS>0) AND ((SELECT CodigoDx from @TablaDiag where Numero=@Contador)<>'') 

			-- Actualizar Hospitalizacion autorizada o programada
			If @IAfiliado<>'' 
			BEGIN                               
                    UPDATE PLANESMEDICOS..aut_hospitalaria_enc 
					SET IdCliente= @ICliente, SerieAdmision=@SerieAdmision, Admision=@Admision
                    WHERE Empresa=@IEmpresaU and tipo_hospitalizacion = 3 
					AND IdAfiliado = @IAfiliado
					AND Status='A'
					AND Codigo_Hospital=@IHospital			
			END --If @IAfiliado<>'' 


		END	  -- If ISNULL(@IHabitacion,'')<>'' AND (@IOpcion = 'I' OR @IOpcion = 'R'  OR @IOpcion = 'U' OR @ITipoPaquete=1) 
		 
		IF @IOpcion = 'Q' -- Paquete Quirúrgico
		BEGIN	--  Actualiza en Hospital..PqtPacientes el número de admisión en caso de que ya posea un paquete asignado...
		  If @IPQTPacienteID=0 SET @ErrRaise=@ErrRaise + 'Error: No se recibió el id del paquete PQT asignado al paciente|' 
		  
          UPDATE HOSPITAL..PQTPaciente SET SerieAdmision = @SerieAdmision, Admision = @Admision
          WHERE (SerieAdmision IS NULL) AND (Admision IS NULL) 
		  AND Empresa = @IEmpresaU AND CodigoPaciente = @ICodigoPac
		  AND Id = @IPQTPacienteID
		END -- IF @IOpcion = 'Q' -- Paquete Quirúrgico



		-- Si es RN ver si tiene paquete
		-- Puede traer ya un paquete escogido, aqui se inserta
		-- debe venir definidas estos parametros externos si lo tiene
		/*
		@IPaqueteBebe INT=0, 
        @IMedaxBebe BIT NULL,
        @IHospitalBebeCodigoBase VARCHAR(3),
        @IPrecioPaqueteBebe MONEY 
		*/

		If (@IOpcion = 'R')
		BEGIN
			If @IPaqueteBebe > 0
			Insert Hospital..PQTPaciente (Empresa, CodigoPaciente, IdPQT, Status, SerieAdmision, Admision, 
										Medax, Hospital, Precio, Fecha, Usuario) 
								Values (@IEmpresaU, @ICodigoPac, @IPaqueteBebe, 'A', @SerieAdmision, @Admision,
								@IMedaxBebe, @IHospitalBebeCodigoBase, @IPrecioPaqueteBebe, Getdate(), @Usuario)

			-- En caso de ser RN de Salud siempre insertara coberturas para cubrir al bebe con seguro
            --ActualizarAdmisionRn( SerieAdmisionRaiz, AdmisionRaiz, SerieAdmision.Text, strToint(Codigo.Text),   UsuarioActual);
			execute Hospital.dbo.sp_generar_cobertura_rn
									@i_Serie_Admision=@ISerieAdmRaiz,
									@i_codigo_admision=@IAdmisionRaiz,
									@i_Serie_admision_rn=@SerieAdmision,
									@i_codigo_admision_rn=@Admision,
									@i_Usuario=@Usuario
		END


		IF @IOpcion = 'U'
		BEGIN  --Admisión Emergencia
		    Select @Producto=ProductoEmergencia From ContaDB..EmpresasDefaults With (nolock) Where Empresa = @IEmpresaReal

			-- Tipo de Orden por Hospital y correlativo orden  ------- REVISAR SI AUN SE USA O SE BUSCA CON UN MAX
			SELECT @TipoOrden = OrdenesTipos.Codigo, @EmerOrden = x.Siguiente
			FROM  Hospital..OrdenesTipos WITH (nolock) INNER JOIN
				(SELECT Empresa, Tipo, ISNULL(MAX(Codigo), 0) + 1 AS Siguiente
				FROM    Hospital..Ordenes WITH (nolock)
				GROUP BY Empresa, Tipo) AS x ON x.Empresa = OrdenesTipos.Empresa AND OrdenesTipos.Codigo = x.Tipo
			WHERE        (OrdenesTipos.Codigo LIKE 'EM%')
			AND (OrdenesTipos.Empresa = @IEmpresaU )
			AND (OrdenesTipos.Activo = 'S')
			AND (OrdenesTipos.Hospital = @IHospital)

			If ISNULL(@TipoOrden, '')='' SET @ErrRaise=@ErrRaise + 'Error: No se encontró el tipo de orden de emergencia|' 

			Select @Categoria=Categoria, @Costo=Case when Costeo='P' then CostoPromedio else CostoUltimo end
			from INVENTARIO..PRODUCTOS (nolock)
			Where Empresa = @IEmpresaU
			and Codigo = @Producto

            --Recuperar el precio de la habitación del nivel de la admision
			Select @Precio=Precio, @UMedida=UnidadMedida, @Factor=Factor
			from Inventario..ProductosPrecios (nolock)
			Where Empresa = @IEmpresaU
			and SubNivel = 1  and Nivel = @NivelPrecios
			and Producto = @Producto

            -- Insertar Orden en la tabla Ordenes
            INSERT INTO Ordenes(Empresa, Tipo, Codigo, Fecha, SerieAdmision, Admision, Usuario, Status, 
								EmpresaReal, EmpresaUnif, Habitacion) 
            VALUES (@IEmpresaU, @TipoOrden, @EmerOrden, Getdate(), @SerieAdmision, @Admision, @Usuario, 'P',
								@IEmpresaReal, @IEmpresaU, '' )

			SET @Linea = 1

            --Insertar Cargo (Habitacion de emergencia)
			 INSERT INTO Cargos (Empresa, TipoOrden, Orden, Linea, SerieAdmision, Admision, Fecha,
								Producto, Categoria, Valor, Cantidad, Status, Paciente, PrecioUnitario, 
								SubNivelPrecios, Factor, Costo, UnidadMedida, EmpresaReal, EmpresaUnif, 
								CostoHospital, Grupocomplementarios) 
						Values (@IEmpresaU, @TipoOrden, @EmerOrden, @Linea, @SerieAdmision, @Admision, Getdate(),
							@Producto, @Categoria, @Precio, 1, 'P', @ICodigoPac, @Precio,	
							1, @Factor, @Costo, @UMedida, @IEmpresaReal, @IEmpresaU,
							@Costo, @Producto )
			-- Los Costos y CostoHospital son lo mismo ya que son servicios

			-- COMPLEMENTARIOS --
			DECLARE @TablaComp TABLE (ProductoSecundario VARCHAR(13), Cantidad FLOAT, MultiplicarXCargos CHAR(1), Cargable CHAR(1), 
										PermitirDuplicidad CHAR(1), Unico CHAR(1), UnaVezXArea CHAR(1))
			INSERT INTO @TablaComp
			SELECT ProductoSecundario, Cantidad, MultiplicarXCargos, Cargable, PermitirDuplicidad, Unico, UnaVezXArea
			FROM Inventario..Complementarios (nolock)
			WHERE Empresa = @IEmpresaU
			AND ProductoPrimario = @Producto
			AND Activo = 'S'

			SELECT @REGISTROS = COUNT(*) FROM @TablaComp
			SET @CONTADOR=1
			IF @REGISTROS>0 
			BEGIN                  
				WHILE @CONTADOR <= @REGISTROS
				BEGIN
					SET @Producto = (SELECT ProductoSecundario FROM (SELECT ROW_NUMBER() OVER (ORDER BY ProductoSecundario) AS RowNum, ProductoSecundario FROM @TablaComp) AS T WHERE RowNum = @CONTADOR)

					--Recuperar la categoria del producto habitacion a cargar
					Select @Categoria=Categoria, @Costo=Case when Costeo='P' then CostoPromedio else CostoUltimo end
					from INVENTARIO..PRODUCTOS (nolock)
					Where Empresa = @IEmpresaU
					and Codigo = @Producto

					--Recuperar el precio de la habitación del nivel de la admision
					Select @Precio=Precio, @UMedida=UnidadMedida, @Factor=Factor
					from Inventario..ProductosPrecios (nolock)
					Where Empresa = @IEmpresaU
					and SubNivel = 1  and Nivel = @NivelPrecios
					and Producto =  @Producto
			  
                    SET @Linea = @Linea + 1 -- La línea de cargo es diferente a la linea del contador

                    -- Insertar Cargo (Complementario)
                    INSERT INTO Cargos (Empresa, TipoOrden, Orden, Linea, SerieAdmision, Admision, Fecha,
									Producto, Categoria, Valor, Cantidad, Status, Paciente, PrecioUnitario, 
									SubNivelPrecios, Factor, Costo, UnidadMedida, EmpresaReal, EmpresaUnif, 
									CostoHospital, Grupocomplementarios)                                 
					VALUES (@IEmpresaU, @TipoOrden, @EmerOrden, @Linea, @SerieAdmision, @Admision, Getdate(),
							@Producto, @Categoria, @Precio, 1, 'P', @ICodigoPac, @Precio,	
							1, @Factor, @Costo, @UMedida, @IEmpresaReal, @IEmpresaU,
							@Costo, @Producto )
                
                SET @CONTADOR = @CONTADOR + 1;
				END -- fin del while
			END    -- fin de complementario

		END
	
	   -- Rutina de vacunas en Pacientes
	   If @IVacuna1>0 OR @IVacuna2>0 OR @IVacuna3>0
	   Begin
	       SET @Contador=1	
		   WHILE @Contador <= 3	
		   BEGIN
			   SELECT @VACUNA = CASE WHEN @Contador=1 THEN @IVacuna1
									 WHEN @Contador=2 THEN @IVacuna2
									 WHEN @Contador=3 THEN @IVacuna3 END			   
			   If @VACUNA>0 
						EXEC Hospital..sp_his_vacunas_paciente 
						@i_vacunado ='S',
						@i_paciente =@ICodigoPac,
						@i_dosis =@Contador,
						@i_vacuna =@VACUNA,
						@i_empresa =@IEmpresaU,
						@i_usuario =@Usuario;
				SET @Contador = @Contador + 1
			END
	   END
	   ELSE
		   EXEC Hospital..sp_his_vacunas_paciente 
		   @i_vacunado ='N',
		   @i_paciente =@ICodigoPac,
		   @i_dosis =0,
		   @i_vacuna =0,
		   @i_empresa =@IEmpresaU,
		   @i_usuario =@Usuario;

	  /* -- Esto sería para una actualización de una admisión
	  UPDATE HOSPITAL.DBO.ADMISIONES SET DiagnosticoCOVID =:DIAGNOSTICADOCOVID
      where empresa =:EMPRESA AND SERIE =:SERIE AND CODIGO =:NUMEROADMISION
	  */

	END 	-- fin de la sección general


	If @ErrRaise <> '' SET @ErrRaiseNum = 1 ELSE SET @ErrRaise='Se ha generado la admisión ' + @SerieAdmision + '-' + CAST(@Admision AS VARCHAR(12))
	SELECT @ErrRaiseNum AS Codigo, @ErrRaise AS Descripcion, @ErrRaiseNum AS tipo_error

END TRY
BEGIN CATCH		
	SET @ErrRaise = @ErrRaise + ' - ' + ERROR_MESSAGE()
	SET @ErrRaiseNum = ERROR_NUMBER() 

	select @ErrRaiseNum AS Codigo, @ErrRaise as Descripción, 16 as tipo_error
	
	--IF @ErrRaise IS NOT NULL RAISERROR( @ErrRaise, 16, @ErrRaiseNum) 

END CATCH
END


/*
Analisis de delphi de las admisiones Externos tipo paquete
SI ES EXTERNO Y Q
   /// En caso de ser una admision de tipo "Paquete" entonces,
   /// intentaremos grabar dicho paquete en Hospital..Ordenes y Cargos.
	AGREGAR PAQUETE RALA (pMkeSynapse)
	Segun el paquete escogido puede cambiar el nivel de precio
	
	ADMISIONES AFTER OPEN

    //inserta los cargos del paquete
	En el siguiente procedimiento se invoca a otro formulario 
	y se le pasa los parámetros

    self.BtnPaquetesCargos.Click;
		  PaquetesRaLaFrm.Empresa   := Empresa;
		  PaquetesRaLaFrm.Sucursal  := Hospital;
		  PaquetesRaLaFrm.SucCargo  := HospSucursal;
		  PaquetesRaLaFrm.SerieAdm  := self.Table1Serie.Value;
		  PaquetesRaLaFrm.Admision  := self.Table1Codigo.Value;

		  PaquetesRaLaFrm.ShowModal;

		  En el formShow se llama al boton self.BtnGenera.Click

				//obtiene parametros
				self.Admisiones.Close;
				self.qAdmisiones.Parameters.Items[0].Value := Empresa;
				self.qAdmisiones.Parameters.Items[1].Value := SerieAdm;
				self.qAdmisiones.Parameters.Items[2].Value := Admision;
				self.Admisiones.Open;
				     self.Admisiones.SQL =
						select Empresa, EmpresaReal, Usuario, Serie as SerieAdmision, Codigo as Admision, 
						Medico as Ajeno, Paquete, Interno, 'P' as Status, Paciente
						from Admisiones
						where Empresa  = :Empresa
						and Serie = :SerieAdmision
						and Codigo = :Admision

						Al abrir admisiones y llenar el cliendataset tenemos el evento
						AdmisionesAfterOpen
									self.Paquetes.Close;
									self.qOrdenesPaq.Close;

									self.qPaquetes.Parameters.Items[0].Value := Sucursal;
									self.qPaquetes.Parameters.Items[1].Value := self.AdmisionesEmpresa.Value;
									self.qPaquetes.Parameters.Items[2].Value := self.AdmisionesPaquete.Value;

									self.qOrdenesPaq.Parameters.Items[0].Value := Sucursal;
									self.qOrdenesPaq.Parameters.Items[1].Value := self.AdmisionesEmpresa.Value;
									self.qOrdenesPaq.Parameters.Items[2].Value := self.AdmisionesPaquete.Value;

									self.pMkeSynapse.Parameters.Items[6].Value := 'P';
									self.pMkeSynapse.Parameters.Items[4].Value := Sucursal;
									self.pMkeSynapse.Parameters.Items[1].Value := self.AdmisionesEmpresa.Value;
									self.pMkeSynapse.Parameters.Items[2].Value := self.AdmisionesSerieAdmision.Value;
									self.pMkeSynapse.Parameters.Items[3].Value := self.AdmisionesAdmision.Value;
									self.pMkeSynapse.Parameters.Items[5].Value := self.AdmisionesUsuario.Value;

									self.Paquetes.Open;
									self.qOrdenesPaq.Open;    Estos dos objetos los referencio abajo con ***

				//crea las ordenes segun tipo
				  self.OrdenesPaq.First;
				  Repeat
						self.upOrdenes.Parameters.Items[1].Value := self.OrdenesPaqOrden.Value;
						self.upOrdenes.Parameters.Items[3].Value := self.AdmisionesAdmision.Value;
						self.upOrdenes.Parameters.Items[0].Value := self.OrdenesPaqTipoOrden.Value;
						self.upOrdenes.Parameters.Items[2].Value := self.AdmisionesSerieAdmision.Value;
					upOrdenes=	Insert Into Ordenes (Empresa, EmpresaReal, Tipo, Codigo, Fecha, SerieAdmision, Admision, Medico, EmpresaUnif, Interno, Usuario, Status)
								Select Empresa, EmpresaReal, :TipoOrd, :Orden, getdate(), Serie, Codigo, Medico, Empresa, Interno, Usuario, 'P' as Status
								From Admisiones
								where Serie = :SerieAdmision
								and Codigo = :Admision

						nNumOrden := self.OrdenesPaqOrden.Value +1;
						sCommand := ' Update CorrelOrdenes Set Siguiente = '+ IntToStr(nNumOrden)
								  + ' Where Empresa = '+ quotedstr(self.AdmisionesEmpresa.Value)
								  + '   and   Tipo  = '+ quotedstr(self.OrdenesPaqTipoOrden.Value);
						self.upPaquetes.CommandText := sCommand;
				  Until lInserta and self.OrdenesPaq.Eof;

				  //inserta los cargos
				  TRY
				      while not(self.OrdenesPaq.Eof) do
						begin
						  self.pOrdenesBar.Position := self.OrdenesPaq.RecNo;

						  while not(self.Paquetes.Eof)
						  and (self.OrdenesPaqTipoOrden.Value = self.PaquetesTipoOrden.Value) do
						  begin
						  self.pCargosBar.Position := self.pCargosBar.Position +1;
						  sCommand := ' Insert into Cargos (Empresa, TipoOrden, Orden, Linea, '
									+ '	SerieAdmision, Admision, Producto, Categoria, Valor,  '
									+ ' Cantidad, PrecioUnitario, Costo, CostoHospital,   '
									+ ' Status, Paciente, Ajeno, SubNivelPrecios, Factor, '
									+ '	UnidadMedida, Sucursal, EmpresaReal, EmpresaUnif, '
									+ '	CategoriaPaq, Fecha)  Values('
									+ ' '+ quotedstr(self.AdmisionesEmpresa.Value)
									+ ','+ quotedstr(self.OrdenesPaqTipoOrden.Value)
									+ ','+ self.OrdenesPaqOrden.AsString
									+ ','+ self.PaquetesLinea.AsString
									+ ','+ quotedstr(self.AdmisionesSerieAdmision.Value)
									+ ','+ self.AdmisionesAdmision.AsString
									+ ','+ quotedstr(self.PaquetesProducto.Value)
									+ ','+ quotedstr(self.PaquetesCategoria.Value)
									+ ','+ self.PaquetesValor.AsString
									+ ','+ self.PaquetesCantidad.AsString
									+ ','+ self.PaquetesPrecioUnitario.AsString
									+ ','+ self.PaquetesCostoPromedio.AsString
									+ ','+ self.PaquetesCostoPromedio.AsString
									+ ','+ quotedstr(self.AdmisionesStatus.Value)
									+ ','+ self.AdmisionesPaciente.AsString
									+ ','+ self.AdmisionesAjeno.AsString
									+ ','+ self.PaquetesSubNivelPrecios.AsString
									+ ','+ self.PaquetesFactor.AsString
									+ ','+ quotedstr(self.PaquetesUnidadMedida.Value)
									+ ','+ quotedstr(SucCargo)
									+ ','+ quotedstr(self.AdmisionesEmpresaReal.Value)
									+ ','+ quotedstr(self.AdmisionesEmpresa.Value)
									+ ','+ quotedstr(self.PaquetesCategoPaq.Value)
									+ ','+ quotedstr(formatdatetime('yyyymmdd hh:mm:ss', now));
							self.upPaquetes.CommandText := sCommand + ')';
							self.upPaquetes.Execute;
							self.Paquetes.Next;
					  end;{while paquetes}
					  self.OrdenesPaq.Next;
					end;{while ordenes}

					self.HOSPITAL.CommitTrans;
					self.lMensaje.Caption := 'Cargos creados exitosamente';

-----------------------------------------------------------------------------------------------------------

***
--qpaquetes
Select left(concat(rtrim(TipoDeOrdenCargos), h.CodigoHab),3) as TipoOrden, 
	Linea, q.Producto, p.Nombre, p.Categoria, 	Valor, Cantidad, CostoPromedio, 
	q.PrecioUnitario, q.SubNivelPrecios, q.Factor, q.UnidadMedida,
  q.Categoria as CategoPaq
From Paqueteslineas q (nolock)
Inner join Inventario..Productos p (nolock) on (p.empresa = q.empresa and p.codigo = q.producto)
Inner join Categorias	c (nolock) on (c.empresa = p.empresa and c.codigo = p.categoria)
Inner join NominaDb..EmpHospital h (nolock) on (h.empresa = q.empresa and h.Codigo = :Sucursal)
Left join CorrelOrdenes o (nolock) on (o.empresa = q.empresa and o.tipo = left(concat(rtrim(TipoDeOrdenCargos), h.CodigoHab),3))
Where q.Empresa = :Empresa
and q.Paquete = :Paquete
Order by q.paquete, TipoOrden, Linea

-- qOrdenesPaq
Select left(concat(rtrim(TipoDeOrdenCargos), h.CodigoHab),3) as TipoOrden,  isnull(o.siguiente,1) as Orden
From Paqueteslineas q (nolock)
inner join Inventario..Productos p (nolock) on (p.empresa = q.empresa and p.codigo = q.producto)
inner join Categorias	c (nolock) on (c.empresa = p.empresa and c.codigo = p.categoria)
inner join NominaDb..EmpHospital h (nolock) on (h.empresa = q.empresa and h.Codigo = :Sucursal)
left join CorrelOrdenes o (nolock) on (o.empresa = q.empresa and o.tipo = left(concat(rtrim(TipoDeOrdenCargos), h.CodigoHab),3))
where q.Empresa = :Empresa
and q.Paquete = :Paquete
Group by TipoDeOrdenCargos, CodigoHab, siguiente
Order by TipoDeOrdenCargos
*/
GO



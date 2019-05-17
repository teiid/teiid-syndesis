package org.komodo.rest;

import javax.annotation.PostConstruct;

import org.glassfish.jersey.server.ResourceConfig;
import org.komodo.rest.cors.KCorsFilter;
import org.komodo.rest.cors.KCorsHandler;
import org.komodo.rest.service.KomodoConnectionService;
import org.komodo.rest.service.KomodoDataserviceService;
import org.komodo.rest.service.KomodoDriverService;
import org.komodo.rest.service.KomodoImportExportService;
import org.komodo.rest.service.KomodoMetadataService;
import org.komodo.rest.service.KomodoSearchService;
import org.komodo.rest.service.KomodoUtilService;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.rest.swagger.RestDataserviceConverter;
import org.komodo.rest.swagger.RestPropertyConverter;
import org.komodo.rest.swagger.RestServiceCatalogDataSourceConverter;
import org.komodo.rest.swagger.RestVdbConditionConverter;
import org.komodo.rest.swagger.RestVdbConverter;
import org.komodo.rest.swagger.RestVdbDataRoleConverter;
import org.komodo.rest.swagger.RestVdbImportConverter;
import org.komodo.rest.swagger.RestVdbMaskConverter;
import org.komodo.rest.swagger.RestVdbModelConverter;
import org.komodo.rest.swagger.RestVdbModelSourceConverter;
import org.komodo.rest.swagger.RestVdbPermissionConverter;
import org.komodo.rest.swagger.RestVdbTranslatorConverter;
import org.komodo.rest.swagger.RestVirtualisationStatusConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import io.swagger.converter.ModelConverters;
import io.swagger.jaxrs.config.BeanConfig;
import io.swagger.jaxrs.listing.ApiListingResource;
import io.swagger.jaxrs.listing.SwaggerSerializers;

@Component
public class JerseyConfig extends ResourceConfig {

    @Value("${spring.jersey.application-path:/}")
    private String apiPath;

    @Value("${server.context-path:/}")
    private String context;

	public JerseyConfig() {
	   register(KomodoConnectionService.class);
       register(KomodoExceptionMapper.class);
       register(KomodoUtilService.class);
       register(KomodoDataserviceService.class);
       register(KomodoConnectionService.class);
       register(KomodoDriverService.class);
       register(KomodoVdbService.class);
       register(KomodoSearchService.class);
       register(KomodoMetadataService.class);
       register(KomodoImportExportService.class);
       register(AuthHandlingFilter.class);

       KCorsFilter corsHandler =  new KCorsFilter();
       corsHandler.getAllowedOrigins().add("*");
       corsHandler.setAllowedHeaders(KCorsHandler.ALLOW_HEADERS);
       corsHandler.setAllowCredentials(true);
       corsHandler.setAllowedMethods(KCorsHandler.ALLOW_METHODS);
       corsHandler.setCorsMaxAge(1209600);
       register(corsHandler);
	}

	@PostConstruct
    public void init() {
        // Available at localhost:port/swagger.json
        this.register(ApiListingResource.class);
        this.register(SwaggerSerializers.class);


        BeanConfig config = new BeanConfig();
        config.setConfigId("teiid-komodo");
        config.setTitle("Teiid Komodo Server API");
        config.setVersion("v1");
        config.setContact("Teiid");
        config.setSchemes(new String[] {"https"});
        config.setBasePath(this.context+"/"+this.apiPath);
        config.setResourcePackage("org.komodo.rest.service");
        config.setPrettyPrint(true);
        config.setScan(true);

        //
        // Add converters for display of definitions
        //
        ModelConverters converters = ModelConverters.getInstance();
        converters.addConverter(new RestPropertyConverter());
        converters.addConverter(new RestVdbConditionConverter());
        converters.addConverter(new RestVdbConverter());
        converters.addConverter(new RestVdbDataRoleConverter());
        converters.addConverter(new RestVdbImportConverter());
        converters.addConverter(new RestVdbMaskConverter());
        converters.addConverter(new RestVdbModelConverter());
        converters.addConverter(new RestVdbModelSourceConverter());
        converters.addConverter(new RestVdbPermissionConverter());
        converters.addConverter(new RestVdbTranslatorConverter());
        converters.addConverter(new RestDataserviceConverter());
        converters.addConverter(new RestServiceCatalogDataSourceConverter());
        converters.addConverter(new RestVirtualisationStatusConverter());
    }
}
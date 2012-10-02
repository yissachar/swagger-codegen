package com.wordnik.swagger.runtime.common;

/**
 * Created with IntelliJ IDEA.
 * User: ramesh
 * Date: 10/1/12
 * Time: 10:27 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ServiceLocator {

    String getServerInstance(String serviceType);
}

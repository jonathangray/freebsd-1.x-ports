/*
 * $Id: epsviewe.h,v 1.1 1994/06/01 06:51:38 asami Exp $
 *
 */

#import <appkit/Application.h>
#import <appkit/graphics.h>
#import <dpsclient/dpsclient.h>
#import <appkit/Window.h>

@interface EpsViewer:Application
{
	id theNewWin;
}

- windowCreate:(NXCoord)width Height:(NXCoord)height;
- (NXRect *)nextRectForWidth:(NXCoord)width Height:(NXCoord)height;

@end


